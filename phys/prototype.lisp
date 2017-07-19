(in-package :phys)

(defenum direction (north northeast east southeast south southwest west northwest)
  () (:documentation "The direction of a Factorio entity.

0..7 where 0 is north, 7 is northwest."))
(export 'direction)
(export (tags 'direction))

(deftype connector-io () '(member :both :in :out))

(defclass connector-prototype ()
  ((position :type (simple-vector 2) :accessor connector-position
             :documentation "#(x y)"
             :initarg :position)
   (io :type connector-io :accessor connector-io
       :documentation ":both, :in or :out -- connector read/write direction."
       :initarg :io)
   (range :type number :accessor connector-range
          :documentation "Wire range"
          :initarg :range))
  (:documentation "A Factorio circuit network connector."))

(defmethod print-object ((obj connector-prototype) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (position io range) obj
      (format stream "~<~A ~S range=~A~@:>"
              (list position io range)))))

(defclass entity-prototype ()
  ((name :type simple-string :accessor prototype-name
         :initarg :name
         :documentation "Name")
   (size :type (simple-vector 2) :accessor prototype-size
         :initarg :size
         :documentation "#(width height)")
   (needs-power-p :type boolean :accessor prototype-needs-power-p
                  :initarg :needs-power-p
                  :documentation "Does this entity need power?"
                  :initform nil)
   (pole-p :type boolean :accessor prototype-pole-p
           :initarg :pole-p
           :documentation "Does this entity transmit power?"
           :initform nil)
   (connectors :type (vector connector) :accessor prototype-connectors
               :documentation "Vector of connector prototypes"
               :initarg :connectors
               :initform #())))

(defmethod print-object ((obj entity-prototype) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream
            "~<~A~:_ of size ~A~:[~;~:_ (needs power)~]~:[~;~:_ (power pole)~]~@[~:_ with connectors~:_ ~{~A~^~:_ ~}~]~:>"
            (list 
             (prototype-name obj)
             (prototype-size obj)
             (prototype-needs-power-p obj)
             (prototype-pole-p obj)
             (concatenate 'list (prototype-connectors obj))))))

(defvar *prototypes* (make-hash-table :test 'equal))

(defmacro defentity (symbol &rest other-keys
                     &key (name (string-downcase (symbol-name symbol)))
                       size connectors range &allow-other-keys)
  (assert (typep size 'vector) (size) "Must specify entity size")
  `(progn
     (defparameter ,symbol
       (make-instance 'entity-prototype 
                      ,@(delete-from-plist other-keys :name :connectors :range)
                      :name ,name
                      :connectors (vector
                                   ,@(etypecase connectors
                                       (connector-io
                                        (assert (equalp size #(1 1))
                                                (size connectors)
                                                "Must specify connector position for non-unit-size entity")
                                        `((make-instance 'connector-prototype
                                                         :position (vector 0 0)
                                                         :io ,connectors
                                                         :range ,range)))
                                       (list
                                        (loop for c in connectors
                                              collect
                                              `(make-instance
                                                'connector-prototype
                                                :position (vector ,(first c) ,(second c))
                                                :io ,(or (third c) :both)
                                                :range ,(cond ((typep (third c) 'number)
                                                               (third c))
                                                              ((typep (fourth c) 'number)
                                                               (fourth c))
                                                              (t range)))))))))
     (setf (gethash ,name *prototypes*) ,symbol)))
