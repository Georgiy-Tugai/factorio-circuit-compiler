(in-package :lua-runtime)

;; -------------------------------------------------------------------
;;                             Meta-types
;; -------------------------------------------------------------------

(defclass lua-type ()
  ((metatable :type lua-table
              :allocation :class
              :accessor lua-metatable
              :initform nil)))

(defgeneric lua-object-to-hashable (object)
  (:documentation "Coerce a Lua object to a EQUAL-able value, for things like hash tables"))
(defgeneric lua-to-lisp (object &key &allow-other-keys)
  (:documentation "Coerce a Lua object to a Lisp object."))
(defmethod print-object ((object lua-type) stream)
  (format stream "~S" (lua-to-lisp object
                                   :null 'lua-nil
                                   :false 'lua-false
                                   :true 'lua-true)))

;; -------------------------------------------------------------------
;;                      Null and boolean types
;; -------------------------------------------------------------------

(defclass lua-null (lua-type) ())
(defvar lua-nil (make-instance 'lua-null))
(defmethod print-object ((object lua-null) stream)
  (if (eq object lua-nil)
      (format stream "~S" 'lua-nil)
      (call-next-method)))
(defmethod lua-to-lisp ((object lua-null) &key (null nil)) null)

(defclass lua-boolean (lua-type)
  ((value :type boolean :initarg :value)))
(defvar lua-true (make-instance 'lua-boolean :value t))
(defvar lua-false (make-instance 'lua-boolean :value nil))
(defmethod print-object ((object lua-boolean) stream)
  (cond
    ((eq object lua-true) (format stream "~S" 'lua-true))
    ((eq object lua-false) (format stream "~S" 'lua-false))
    (t (call-next-method))))
(defmethod lua-to-lisp ((object lua-boolean) &key (false nil) (true t))
  (if (slot-value object 'value) true false))

;; -------------------------------------------------------------------
;;                Trivial types (number and string)
;; -------------------------------------------------------------------

(defclass lua-number (lua-type)
  ((value :type double-float)))
(defmethod initialize-instance :after ((instance lua-number) &key (value 0.0d0) &allow-other-keys)
  (setf (slot-value instance 'value)
        (coerce value 'double-float)))
(defmethod lua-object-to-hashable ((object lua-number)) (slot-value object 'value))
(defmethod lua-to-lisp ((object lua-number) &key) (slot-value object 'value))

(defclass lua-string (lua-type)
  ((value :type string :initarg :value)))
(defmethod lua-object-to-hashable ((object lua-string)) (slot-value object 'value))
(defmethod lua-to-lisp ((object lua-string) &key) (slot-value object 'value))

;; -------------------------------------------------------------------
;;                      Reverse hashtable coercion
;; -------------------------------------------------------------------

(defun lua-object-from-hashable (object)
  "Coerce a Lua object or simplified Lua object (see `lua-object-to-hashable') to the original object."
  (if (typep object 'lua-type)
      object
      (etypecase object
        (double-float (make-instance 'lua-number :value object))
        (string (make-instance 'lua-string :value object)))))

;; -------------------------------------------------------------------
;;                          Complex types
;; -------------------------------------------------------------------

(defclass lua-function (lua-type)
  ((value :type function)))
;;(defmethod lua-to-lisp ((object lua-function) &key) (slot-value object 'value))

(defclass lua-userdata (lua-type)
  ((metatable :allocation :instance)))

(defclass lua-thread (lua-type)
  ((value :type green-threads::thread)))
(defmethod lua-to-lisp ((object lua-thread) &key) (slot-value object 'value))

(defclass lua-table (lua-type)
  ((value :type hash-table :initform (make-hash-table :test 'equal))
   (metatable :allocation :instance)))
(defmethod lua-to-lisp ((object lua-table) &key (shallow t))
  (if shallow
      (slot-value object 'value)
      (let ((ret (make-hash-table)))
        (loop for k being the hash-keys in (slot-value object 'value)
                using (hash-value v)
              do (setf (gethash (lua-to-lisp k)
                                ret)
                       (lua-to-lisp v)))
        ret)))
