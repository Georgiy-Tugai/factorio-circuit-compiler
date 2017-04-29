(in-package :factorio-circuits.netlist-dsl)
(defun arithmetic (a b op out &rest inputs)
  (let ((ent (make-instance 'entity-combinator-arithmetic)))
    (setf (entity-combinator-signal-a ent) a
          (entity-combinator-signal-b ent) b
          (entity-combinator-operator ent) op
          (entity-combinator-signal-output ent) out)
    (values ent inputs)))

(defun conditional (a b op out value-p &rest inputs)
  (let ((ent (make-instance 'entity-combinator-decider)))
    (setf (entity-conditional-signal-a ent) a
          (entity-conditional-signal-b ent) b
          (entity-conditional-operator ent) op
          (entity-combinator-signal-output ent) out
          (entity-combinator-value-p ent) value-p)
    (values ent inputs)))

(defun -apply-connect (func param)
  (multiple-value-bind (ent inputs) (apply func param)
    (let ((col :red))
      (mapc (lambda (x)
              (cond
                ((member x '(:red :green)) (setf col x))
                ((eql x :self) (entity-connect ent ent col))
                (t (entity-connect x ent col))))
            inputs))
    ent))

(defmacro netlist-dsl (&body body)
  `(labels ((:arith (&rest param)
              (-apply-connect #'arithmetic param))
            (:cond (&rest param)
              (-apply-connect #'conditional param))
            (:const (&rest param)
              (let ((ent (make-instance 'entity-combinator-constant)))
                (setf (entity-combinator-constants ent) param)
                ent))
            (:self (color ent)
              (entity-connect ent ent color)
              ent)
            (:other (type)
              (make-instance 'entity-other :type type))
            (:conn (&rest param)
              (with-simple-restart (skip "Skip ~A" param)
                (apply #'entity-connect param)))
            (:conn-inputs (&rest param)
              (with-simple-restart (skip "Skip ~A" param)
                (apply #'entity-connect-inputs param)))
            (:conn-outputs (&rest param)
              (apply #'entity-connect-outputs param)))
     ,@body))
