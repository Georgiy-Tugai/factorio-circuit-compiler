(in-package :lua-types)

;; -------------------------------------------------------------------
;;                             Meta-types
;; -------------------------------------------------------------------

(defclass lua-type ()
  ((metatable :type lua-table
              :allocation :class
              :accessor lua-metatable
              :initform nil)))

(defgeneric lua-to-lisp (object &key &allow-other-keys)
  (:documentation "Coerce a Lua object to a Lisp object."))
(defmethod print-object ((object lua-type) stream)
  (format stream "~S" (lua-to-lisp object)))
(defmethod lua-type-name (obj))

(defmacro def-native-type (typespec string)
  (let ((var (intern (format nil "*LUA-~:@(~A~)-METATABLE*" string))))
    `(progn
     (defvar ,var nil)
     (defmethod lua-metatable ((obj ,typespec))
       ,var)
     (defmethod (setf lua-metatable) (new-value (obj ,typespec))
       (setf ,var new-value))
     (defmethod lua-type-name ((obj ,typespec)) ,string)
     (defmethod lua-to-lisp ((obj ,typespec) &key) obj))))

;; -------------------------------------------------------------------
;;                      Null and boolean types
;; -------------------------------------------------------------------

(def-native-type null "nil")

(defclass lua-false (lua-type) ())
(defvar lua-false (make-instance 'lua-false))
(defvar lua-nil 'lua-nil)

(defmethod print-object ((object lua-false) stream)
  (cond
    ((eq object lua-false) (format stream "~S" 'lua-false))
    (t (call-next-method))))
(defmethod lua-to-lisp ((object lua-false) &key (false nil)) false)

;; Lua's nil could be Lisp nil or the more explicit lua-nil
(defmethod lua-metatable ((obj (eql lua-nil))) (lua-metatable nil))
(defmethod (setf lua-metatable) (new-value (obj (eql lua-nil))) (setf (lua-metatable nil) new-value))
(defmethod lua-type-name ((obj (eql lua-nil))) (lua-type-name nil))
(defmethod lua-to-lisp ((obj (eql lua-nil)) &key) nil)

(def-native-type (eql lua-false) "boolean")

;; Lua's true = t
(defmethod lua-metatable ((obj (eql t))) (lua-metatable lua-false))
(defmethod (setf lua-metatable) (new-value (obj (eql t))) (setf (lua-metatable lua-false) new-value))
(defmethod lua-type-name ((obj (eql t))) (lua-type-name lua-false))
(defmethod lua-to-lisp ((obj (eql t)) &key) t)

(defun lua-boolean (val)
  "DWIMmy conversion of Lisp booleans to Lua booleans."
  (if (and val
           (not (eql val lua-false)))
      (if (eql val lua-nil)
          nil
          val)
      lua-false))
(defun lisp-boolean (val)
  "DWIMmy conversion of Lua booleans to Lisp booleans."
  (not (or (eql val lua-false)
           (eql val lua-nil)
           (null val))))

;; -------------------------------------------------------------------
;;                 Trivial types (shared metatable)
;; -------------------------------------------------------------------

(def-native-type number "number")
(def-native-type string "string")
(def-native-type function "function")

;; -------------------------------------------------------------------
;;                          Complex types
;; -------------------------------------------------------------------

(defclass lua-userdata (lua-type)
  ((metatable :allocation :instance)))

(defclass lua-thread (lua-type)
  ((value :type green-threads::thread)))
(defmethod lua-to-lisp ((object lua-thread) &key) (slot-value object 'value))

(defclass lua-table (lua-type)
  ((value :type hash-table)
   (metatable :allocation :instance)))

(defun translate-weakness (table)
  (let ((weakness nil))
    (when (lua-metatable table)
      (let ((mode (gethash "__mode" (lua-metatable table))))
        (when (find #\k mode)
          (setf weakness :key))
        (when (find #\v mode)
          (setf weakness (if weakness
                             :key-or-value
                             :value)))))))

(defun maybe-rebuild-table (table)
  (let ((w (translate-weakness table)))
    (if (not (slot-boundp table 'value))
        (setf (slot-value table 'value)
              (make-weak-hash-table :weakness w :test 'equal))
        (when (not (eql (hash-table-weakness (slot-value table 'value))
                        w))
          (let ((new-table (make-weak-hash-table :weakness w
                                                 :size (hash-table-count
                                                        (slot-value table 'value)))))
            (loop for k being the hash-keys in (slot-value table 'value)
                    using (hash-value v)
                  do (setf (gethash new-table k) v))
            (setf (slot-value table 'value)
                  new-table))))))

(defmethod lua-to-lisp ((object lua-table) &rest args &key (shallow t))
  (maybe-rebuild-table object)
  (if shallow
      (slot-value object 'value)
      (let ((ret (make-hash-table :test 'equal)))
        (loop for k being the hash-keys in (slot-value object 'value)
                using (hash-value v)
              do (setf (gethash (apply #'lua-to-lisp k args)
                                ret)
                       (apply #'lua-to-lisp v args)))
        ret)))

(defmethod print-object ((object lua-table) stream)
  (format stream "~A" (lua-to-lisp object)))

(defgeneric lua-rawget (table key))
(defgeneric lua-rawset (table key value))

(defmethod lua-rawget ((table lua-table) key)
  (maybe-rebuild-table table)
  (gethash key (slot-value table 'value)))
(defmethod lua-rawset ((table lua-table) key value)
  (maybe-rebuild-table table)
  (setf (gethash key (slot-value table 'value))
        value))
