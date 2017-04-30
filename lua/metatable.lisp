(in-package :lua-metatable)

(defun trymetatable (obj evt)
  (when (lua-metatable obj)
    (gethash evt obj)))
(defun getbinhandler (op1 op2 evt)
  (or (trymetatable op1 evt)
      (trymetatable op2 evt)))

(defmacro def-binary-operators (&rest names)
  `(progn
     ,@(loop for name in names
             append
             (let ((method (intern (format nil "LUA-~A"
                                           (symbol-name name))))
                   (h (gensym)))
               `((defgeneric ,method (op1 op2))
                 (defmethod ,method (op1 op2)
                   (let ((,h (getbinhandler op1 op2 ,(format nil "__~A" name))))
                     (if ,h
                         (lua-call ,h op1 op2)
                         (error "Binary operator ~A not implemented for ~S and ~S"
                                ',name op1 op2))))
                 (export ',method))))))

(defmacro def-numeric-binop (name fun)
  (let ((method (intern (format nil "LUA-~A"
                                (symbol-name name)))))
    `(defmethod ,method ((op1 number) (op2 number))
       (,fun op1 op2))))

(def-binary-operators add sub mul div mod pow concat)
(def-numeric-binop add +)
(def-numeric-binop sub -)
(def-numeric-binop mul *)
(def-numeric-binop div /)
(def-numeric-binop mod mod)
(def-numeric-binop pow expt)

(defmethod lua-concat ((op1 number) (op2 number)) (format nil "~A~A" op1 op2))
(defmethod lua-concat ((op1 number) (op2 string)) (format nil "~A~A" op1 op2))
(defmethod lua-concat ((op1 string) (op2 number)) (format nil "~A~A" op1 op2))
(defmethod lua-concat ((op1 string) (op2 string)) (concatenate 'string op1 op2))

(defgeneric lua-lt (op1 op2))
(defmethod lua-lt :around (op1 op2)
  (lua-boolean (call-next-method)))
(defmethod lua-lt (op1 op2)
  (let ((h (getbinhandler op1 op2 "__lt")))
    (if h (lua-call h op1 op2)
        (error "Less-than not implemented for ~S and ~S" op1 op2))))
(defmethod lua-lt ((op1 number) (op2 number)) (< op1 op2))
(defmethod lua-lt ((op1 string) (op2 string)) (numberp (string< op1 op2)))
(export 'lua-lt)

(defgeneric lua-le (op1 op2))
(defmethod lua-le :around (op1 op2)
  (lua-boolean (call-next-method)))
(defmethod lua-le (op1 op2)
  (let ((h (getbinhandler op1 op2 "__le")))
    (if h (not (lua-call h op1 op2))
        (let ((h2 (getbinhandler op1 op2 "__lt")))
          (lua-call h2 op1 op2)
          (error "Less-than-or-equal not implemented for ~S and ~S" op1 op2)))))
(export 'lua-le)

(defmacro def-unary-operators (&rest names)
  `(progn
     ,@(loop for name in names
             append
             (let ((method (intern (format nil "LUA-~A"
                                           (symbol-name name))))
                   (h (gensym)))
               `((defgeneric ,method (op))
                 (defmethod ,method (op)
                   (let ((,h (trymetatable op ,(format nil "__~A" name))))
                     (if ,h
                         (lua-call ,h op)
                         (error "Unary operator ~A not implemented for ~S"
                                ',name op))))
                 (export ',method))))))

(def-unary-operators unm len)

(defmethod lua-unm ((op number)) (- op))

(defmethod lua-len ((op string)) (length op))
(defmethod lua-len ((op lua-table))
  (if (trymetatable op "__len")
      (call-next-method)
      (hash-table-count (lua-to-lisp op))))

(defgeneric lua-eq (op1 op2))
(defmethod lua-eq :around (op1 op2)
  (lua-boolean (call-next-method)))
(defmethod lua-eq (op1 op2)
  (if (eql op1 op2)
      t
      (when (and (string= (lua-type-name op1) (lua-type-name op2))
                 (typep op1 '(or lua-table lua-userdata))
                 (typep op2 '(or lua-table lua-userdata)))
        (let ((h1 (trymetatable op1 "__eq"))
              (h2 (trymetatable op2 "__eq")))
          (when (eql (lua-eq h1 h2) t)
            (lua-call h1 op1 op2))))))
(defmethod lua-eq ((op1 number) (op2 number)) (= op1 op2))
(defmethod lua-eq ((op1 string) (op2 string)) (string= op1 op2))
(export 'lua-eq)

(defgeneric lua-index (table key))
(defmethod lua-index (table key)
  (let ((h (trymetatable table "__index")))
    (if h
        (if (typep h 'function)
            (funcall h table key)
            (lua-index h key))
        (error "Indexing access not implemented for ~S" table))))
(defmethod lua-index ((table lua-table) key)
  (let ((ret (gethash key (lua-to-lisp table))))
    (if ret ret
        (call-next-method))))
(export 'lua-index)

(defgeneric lua-newindex (table key value))
(defmethod lua-newindex (table key value)
  (let ((h (trymetatable table "__newindex")))
    (if h
        (if (typep h 'function)
            (funcall h table key value)
            (lua-newindex h key value))
        (error "Indexing assignment not implemented for ~S" table))))
(defmethod lua-newindex ((table lua-table) key value)
  (let ((existing (gethash key (lua-to-lisp table))))
    (if existing
        (setf (gethash key (lua-to-lisp table))
              value)
        (if (trymetatable table "__newindex")
            (call-next-method)
            (setf (gethash key (lua-to-lisp table))
                  value)))))
(export 'lua-newindex)

(defgeneric lua-call (func &rest args))
(defmethod lua-call (func &rest args)
  (let ((h (trymetatable func "__call")))
    (if h
        (apply h func args)
        (error "Calling not implemented for ~S" func))))
(defmethod lua-call ((func function) &rest args)
  (apply func args))
(export 'lua-call)
