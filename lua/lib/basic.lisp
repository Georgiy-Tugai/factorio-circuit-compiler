(in-package :lua-runtime)
(cl-interpol:enable-interpol-syntax)

(lua-defun |assert| (v &rest message-or-v)
  (if (lisp-boolean v)
      (values-list (list* v message-or-v))
      (error "assertion failed!")))
(lua-defun |collectgarbage| (&rest args)
  (error "collectgarbage~S not implemented!" args))
(lua-defun |dofile| (filename)
  (error "dofile(~S) forbidden!" filename))
(lua-defun |error| (message &optional (level 1))
  (declare (ignore level))
  (error "~S" message))
(lua-defun |getmetatable| (obj)
  (lua-metatable obj))
(lua-defun |load| (ld &optional source mode env)
  (error "load(~S, ~S, ~S, ~S) not implemented!" ld source mode env))
(lua-defun |loadfile| (&optional filename mode env)
  (error "loadfile(~S, ~S, ~S) not implemented!" filename mode env))

;; Note: builtin next() is allowed to ignore __index metatable!
(lua-defun |next| (table &optional (index lua-nil))
  (let ((h (lua-to-lisp table))
        (i (lua-to-lisp index))
        ret)
    (assert (hash-table-p h) nil "Lua basic library function next expects a table as the first argumnet, not ~S!" table)
    (loop for k being the hash-keys in h
          when (or (not i)
                   ret)
            return (values k (gethash k h))
          when (funcall (hash-table-test h)
                        k i)
            do (setq ret t))))

(lua-defun |pairs| (table) (lua-pairs table))
(lua-defun |ipairs| (table) (lua-ipairs table))

(lua-defun |print| (&rest args)
  (format t #?"~{~A~^\t~}~&" args)
  (values))

(lua-defun |pcall| (f &rest args)
  (handler-case (apply 'lua-call f args)
    (t (condition)
      (values lua-false (format nil "~A" condition)))))

(lua-defun |rawequal| (v1 v2)
  (typecase v1
    (string (when (stringp v2)
              (string= v1 v2)))
    (number (when (numberp v2)
              (= v1 v2)))
    (t (eql v1 v2))))

(lua-defun |rawget| (table index) (lua-rawget table index))
(lua-defun |rawlen| (table)
  (etypecase table
    (lua-table (hash-table-count (lua-to-lisp table)))
    (string (length table))))
(lua-defun |rawset| (table index value) (lua-rawset table index value))

(lua-defun |select| (index &rest args)
  (let ((len (length args)))
    (cond ((and (stringp index)
                (equal index "#"))
           len)
          ((numberp index)
           (values-list
            (subseq args (min len
                              (if (> index 0)
                                  (1- index)
                                  (+ len index)))))))))

(lua-defun |getmetatable| (obj) (lua-metatable obj))
(lua-defun |setmetatable| (obj table)
  (assert (typep obj 'lua-table) nil "Can't set metatable of non-tables from Lua!")
  (assert (not (and (lua-metatable obj)
                    (lua-index (lua-metatable obj) "__metatable")))
          nil "Can't change a protected metatable!")
  (setf (lua-metatable obj) table)
  obj)

(lua-defun |tonumber| (obj &optional base)
  (if (lua-to-lisp base)
      (etypecase obj
        (number obj)
        (string (handler-case
                    (multiple-value-bind (n)
                        (parse-integer obj :radix base)
                      n)
                  (t nil))))
      (handler-case
          (lua-coerce obj 'number :must t)
        (t nil))))

(lua-defun |tostring| (obj) (lua-tostring obj))
(lua-defun |type| (obj) (lua-type-name obj))
(lua-defvar |_VERSION| "CL-Lua 5.2")
(lua-defun |xpcall| (f msgh &rest args)
  (handler-case (apply 'lua-call f args)
    (t (condition)
      (handler-case (lua-call msgh (format nil "~A" condition))
        (t () (values lua-false "error in error handling"))))))
