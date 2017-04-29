(in-package :factorio-circuits.entity)

(defclass entity ()
  ((entity-position :accessor entity-position)
   (entity-symbol :accessor entity-symbol :type symbol :initform (gensym "ENT"))
   (entity-size :accessor entity-size :type cons :initform (cons 1 1) :allocation :class)))

(defclass entity-rotatable (entity)
  ((entity-angle :accessor entity-angle)))

(deftype factorio-signal () 'symbol)
(deftype factorio-value () '(signed-byte 32)) ;XXX: Make sure this matches the game
(deftype factorio-condition-operator () '(member :< :> :=))
(deftype factorio-arithmetic-operator () '(member :+ :- :* :/))

(defclass entity-circuit-input-mixin ()
  ((entity-circuit-input :accessor entity-circuit-input :type cons :initform (cons nil nil))))
(defclass entity-circuit-output-mixin ()
  ((entity-circuit-output :accessor entity-circuit-output :type cons :initform (cons nil nil))))

(defclass entity-other (entity-rotatable entity-circuit-input-mixin entity-circuit-output-mixin)
  ((entity-type :accessor entity-type :initform (error "entity-other type not set!") :initarg :type)))

(defclass entity-conditional-mixin ()
  ((entity-conditional-signal-a :accessor entity-conditional-signal-a :type factorio-signal)
   (entity-conditional-signal-b :accessor entity-conditional-signal-b :type (or factorio-signal factorio-value))
   (entity-conditional-operator :accessor entity-conditional-operator :type factorio-condition-operator)))

(defclass entity-combinator (entity-rotatable entity-circuit-input-mixin entity-circuit-output-mixin) ())

;; --------------------------------------------------------------------------
;; Specifics
;; --------------------------------------------------------------------------
(defclass entity-combinator-decider (entity-combinator entity-conditional-mixin)
  ((entity-combinator-signal-output :accessor entity-combinator-signal-output :type factorio-signal)
   (entity-combinator-value-p :accessor entity-combinator-value-p :type boolean)
   (entity-size :allocation :class :initform (cons 2 1))))
(defclass entity-combinator-arithmetic (entity-combinator)
  ((entity-combinator-signal-a :accessor entity-combinator-signal-a :type factorio-signal)
   (entity-combinator-signal-b :accessor entity-combinator-signal-b :type factorio-signal)
   (entity-combinator-operator :accessor entity-combinator-operator :type factorio-arithmetic-operator)
   (entity-combinator-signal-output :accessor entity-combinator-signal-output :type factorio-signal)
   (entity-size :allocation :class :initform (cons 2 1))))
(defclass entity-combinator-constant (entity entity-circuit-output-mixin)
  (;; (list (cons factorio-signal factorio-value))
   (entity-combinator-constants :accessor entity-combinator-constants)))

(defclass entity-lamp (entity entity-circuit-input-mixin entity-conditional-mixin) ())
