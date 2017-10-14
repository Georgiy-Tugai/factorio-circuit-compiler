(in-package :lua-runtime)
(defun lua-table-constructor (alist)
  (loop for (k . v) in alist
        with table = (make-instance 'lua-table)
        with n = 0
        finally (return table)
        do (lua-rawset table (or k (incf n)) v)))

(defclass lua-symbol-table ()
  ((symbols :type hash-table :initform (make-hash-table :test 'equal) :reader lua-symbol-list)
   (parent :type lua-symbol-table :initform nil :reader lua-symbol-parent)))

(defun invert-case (name)
  (let ((up (string-upcase name))
        (down (string-downcase name)))
    (cond ((equal up name) down)
          ((equal down name) up)
          (t name))))

(defgeneric lua-symbol-find (table name &key fallback))
(defmethod lua-symbol-find ((table lua-symbol-table) name &key fallback)
  (or (loop for tbl = table then (slot-value tbl 'parent) while tbl
            do (multiple-value-bind (val status)
                   (gethash name (slot-value tbl 'symbols))
                 (when status
                   (return val))))
      (case fallback
        (:global (intern (invert-case name) (find-package 'lua)))
        (:gensym (gensym (invert-case name)))
        (t fallback))))

(defgeneric lua-symbol-subtable (table))
(defmethod lua-symbol-subtable ((table lua-symbol-table))
  (let ((new (make-instance 'lua-symbol-table)))
    (setf (slot-value new 'parent) table)
    new))

(defgeneric lua-symbol-new (table name))
(defmethod lua-symbol-new ((table lua-symbol-table) name)
  (aif (gethash name (slot-value table 'symbols))
       it
       (setf (gethash name
                      (slot-value table 'symbols))
             (gensym (invert-case name)))))

(defmacro lua-or (a b)
  (alexandria:once-only (a b)
    `(if (or (null ,a) (eql ,a ,lua-false) (eql ,a ,lua-nil)) ,b ,a)))

(defmacro lua-and (a b)
  (alexandria:once-only (a b)
    `(if (or (null ,a) (eql ,a ,lua-false) (eql ,a ,lua-nil)) ,a ,b)))

(defun lua-not (x)
  (cond
    ((eql x lua-nil) t)
    ((eql x lua-false) t)
    (t lua-false)))

(defun lua-method-call (obj name &rest args)
  (apply #'lua-call (lua-index obj name)
         obj args))

(defvar lua::|_G|
  (let ((table (make-instance 'lua-table)))
    (lua-rawset table "_G" table)
    table)
  "Lua globals table")

(defmacro lua-numeric-for (index (start end &optional step) &body body)
  (alexandria:with-gensyms (-index -end -step)
    `(do ((,-end (lua-coerce ,end 'number :must t))
          (,-step ,(if step
                       `(lua-coerce ,step 'number :must t)
                       1))
          (,-index
           (lua-coerce ,start 'number :must t)
           (+ ,-index ,-step))
          (,index))
         ((or (and (> ,-step 0)
                   (> ,-index
                       ,-end))
              (and (< ,-step 0)
                   (< ,-index
                      ,-end))
              (= ,-step 0)))
       (setf ,index ,-index)
       ,@body)))

(defmacro lua-iterator-for (indexes (&rest iterator) &body body)
  (alexandria:with-gensyms (-f -s -var -var_n)
    `(multiple-value-bind (,-f ,-s ,-var)
         (list* ,@iterator)
       (do ((,-var_n (multiple-value-list (lua-call ,-f ,-s ,-var))
                     (multiple-value-list (lua-call ,-f ,-s (first ,-var_n)))))
           ((not (lua-to-lisp (first ,-var_n) :false lua-false)))
         (multiple-value-bind ,indexes
             (values-list ,-var_n)
           (declare (ignorable ,@indexes))
           ,@body)))))

(defmacro lua-defvar (name val)
  "Define a variable in Lua's global namespace"
  `(lua-rawset lua::|_G|
               ,(symbol-name name)
               ,val))

(defmacro lua-defun (name lambda-list &body body)
  "Define a Lisp function in Lua's global namespace"
  `(lua-rawset lua::|_G|
               ,(symbol-name name)
               (alexandria:named-lambda ,name ,lambda-list
                 ,@body)))
