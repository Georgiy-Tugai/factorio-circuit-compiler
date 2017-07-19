(in-package :phys)

(defmacro prototype-methods ((proto class) &rest slots)
  `(progn
     ,@(loop for s in slots
             collect
             `(defgeneric ,(symbolicate class '- s) (obj)
                (:method ((obj ,class))
                  (,(symbolicate proto '- s) (,(symbolicate class '- 'prototype) obj)))))))

(defclass network ()
  ((connectors :type list :accessor network-connectors :initform nil)))

(defclass connector ()
  ((prototype :type connector-prototype :reader connector-prototype :initarg :prototype)
   (red :type network :accessor connector-red :initform nil)
   (green :type network :accessor connector-green :initform nil)
   (entity :type entity :reader connector-entity :initarg :entity)))
(prototype-methods (connector connector) position io range)

(defmethod print-object ((obj connector) stream)
  (print-unreadable-object (obj stream)
    (format stream "~<~A ~S range=~A red:~A green:~A~@:>"
            (list (connector-position obj)
                  (connector-io obj)
                  (connector-range obj)
                  (length (connector-red obj))
                  (length (connector-green obj))))))

(defclass entity ()
  ((prototype :type entity-prototype :reader entity-prototype
              :initarg :prototype
              :documentation "Entity prototype")
   (position  :type (simple-vector 2) :accessor entity-position
              :documentation "#(x y)"
              :initarg :position)
   (direction :type direction         :accessor entity-direction
              :documentation "0..7 where 0 is north, 7 is northwest"
              :initarg :direction)
   (connectors :type (vector connector) :initform #() :accessor entity-connectors))
  (:documentation "A Factorio entity with position, direction, size and prototype name."))
(export '(entity entity-position entity-direction))

(defmethod print-object ((obj entity) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~<~A~:_ at ~A~:_ facing ~A~@[~:_ with connectors~:_ ~{~A~^~:_ ~}~]~:>"
            (list (entity-name obj)
                  (entity-position obj)
                  (direction (entity-direction obj))
                  (concatenate 'list (entity-connectors obj))))))

(prototype-methods (prototype entity) name size needs-power-p pole-p)

(defmethod initialize-instance :after ((e entity) &key)
  (loop for c across (prototype-connectors (entity-prototype e))
        collect (make-instance 'connector
                               :prototype c
                               :entity e)
          into clist
        finally (setf (entity-connectors e)
                      (apply #'vector clist))))
