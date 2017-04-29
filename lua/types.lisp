(in-package :lua-runtime)

;; -------------------------------------------------------------------
;;                             Meta-types
;; -------------------------------------------------------------------

(defclass lua-type ()
  ((metatable :type lua-table
              :allocation :class
              :accessor lua-metatable)))

;; Weakness type key-or-value are necessary in order to avoid keeping
;; Lua objects alive forever. Weak values (or key-and-value) will not
;; work; a value may have no non-weak references other than indirect
;; ones via hash tables, for example, which would cause map entries to
;; be prematurely collected.
;;
;; Portability can be widened somewhat by using finalizers and weak
;; values.
(defvar lua-complex-type-map (make-weak-hash-table :weakness :key-or-value)
  "Map from 'addresses' (i.e. symbols) back to the complex types they represent.

Necessary to implement things like Lua table iteration, since CL hash
tables cannot (portably) have CLOS objects as keys.")
(defclass lua-complex-type (lua-type)
  ((symbol :type symbol :initform (gensym "LUA"))))
(defmethod initialize-instance :after ((instance lua-complex-type) &key &allow-other-keys)
  (setf (gethash (slot-value instance 'symbol)
                 lua-complex-type-map)
        instance))
(defmacro print-complex-object ((object stream &key (type nil) (identity nil)))
  (once-only (object stream)
    `(print-unreadable-object (,object ,stream :type ,type)
       (when ,identity
         (format ,stream "{~S}" (slot-value ,object 'symbol))))))
(defmethod print-object ((object lua-complex-type) stream)
  (print-complex-object (object stream :type t :identity t)))

(defgeneric lua-object-to-hashable (object)
  (:documentation "Coerce a Lua object to a EQUAL-able value, for things like hash tables"))
(defmethod lua-object-to-hashable ((object lua-complex-type)) (slot-value object 'symbol))
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
(defmethod lua-object-to-hashable ((object lua-null))
  (if (eq object lua-nil)
      lua-nil
      (error "Lua null should be the singleton, ~A is not" object)))
(defmethod print-object ((object lua-null) stream)
  (if (eq object lua-nil)
      (format stream "~S" 'lua-nil)
      (call-next-method)))
(defmethod lua-to-lisp ((object lua-null) &key (null nil)) null)

(defclass lua-boolean (lua-type)
  ((value :type boolean :initarg :value)))
(defvar lua-true (make-instance 'lua-boolean :value t))
(defvar lua-false (make-instance 'lua-boolean :value nil))
(defmethod lua-object-to-hashable ((object lua-boolean))
  (cond
    ((eq object lua-true) 'lua-true)
    ((eq object lua-false) 'lua-false)
    (t (error "Lua booleans should be one of the two singletons, ~A is not one of them" object))))
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
;;          Reverse coercion for trivial and complex types
;; -------------------------------------------------------------------

(defun lua-object-from-hashable (object)
  "Coerce a Lua object or simplified Lua object (see `lua-object-to-hashable') to the original object."
  (if (typep object 'lua-type)
      (progn (assert (not (typep object 'lua-complex-type)))
             object)
      (etypecase object
        (symbol
         (cond 
           ((eq object 'lua-nil) lua-nil)
           ((eq object 'lua-false) lua-false)
           ((eq object 'lua-true) lua-true)
           (t
            (multiple-value-bind (ret status)
                 (gethash object lua-complex-type-map)
               (if status ret
                   (error "Simplified object ~A not found in lua-complex-type-map!"
                          object))))))
        (double-float (make-instance 'lua-number :value object))
        (string (make-instance 'lua-string :value object)))))

;; -------------------------------------------------------------------
;;                          Complex types
;; -------------------------------------------------------------------

(defclass lua-function (lua-complex-type)
  ((value :type function)))
;;(defmethod lua-to-lisp ((object lua-function) &key) (slot-value object 'value))

(defclass lua-userdata (lua-complex-type)
  ((metatable :allocation :instance)))

(defclass lua-thread (lua-complex-type)
  ((value :type green-threads::thread)))
(defmethod lua-to-lisp ((object lua-thread) &key) (slot-value object 'value))

(defclass lua-table (lua-complex-type)
  ((value :type hash-table :initform (make-hash-table :test 'equal))
   (metatable :allocation :instance)))
(defmethod lua-to-lisp ((object lua-table) &key (shallow t) (table-to-alist nil))
  (if shallow
      (if table-to-alist
          (loop for k being the hash-keys in (slot-value object 'value)
                  using (hash-value v)
                collect (list k v))
          (slot-value object 'value))
      (if table-to-alist
          (loop for k being the hash-keys in (slot-value object 'value)
                  using (hash-value v)
                collect (list (lua-to-lisp k :table-to-alist t)
                              (lua-to-lisp v :table-to-alist t)))
          (let ((ret (make-hash-table)))
            (loop for k being the hash-keys in (slot-value object 'value)
                    using (hash-value v)
                  do (setf (gethash (lua-to-lisp k) ret)))))))

(defmethod getmetatable ((obj lua-type))
  (if (slot-boundp obj 'metatable)
      (slot-value obj 'metatable)
      lua-nil))

(defmethod setmetatable ((obj lua-type) table)
  (error "Can't set metatable of a non-table!"))
(defmethod setmetatable ((obj lua-table) table)
  (setf (slot-value obj 'metatable)
        table))

(defmethod rawget ((obj lua-table) key)
  (gethash (lua-object-to-hashable key) (slot-value obj 'value)))
(defmethod rawset ((obj lua-table) key value)
  (setf (gethash (lua-object-to-hashable key) (slot-value obj 'value))
        value))
