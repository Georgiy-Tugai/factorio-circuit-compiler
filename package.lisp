(defpackage :factorio-circuits.app-utils
  (:use :cl)
  (:export :internal-disable-debugger)
  (:export :internal-quit))

(defpackage :factorio-circuits.entity
  (:use :cl)
  (:export
   :factorio-signal
   :factorio-value
   :factorio-condition-operator
   :factorio-arithmetic-operator

   :entity
   :entity-position
   :entity-symbol
   :entity-size
   :entity-rotatable
   :entity-other
   :entity-angle
   :entity-type
   
   :entity-circuit-input-mixin
   :entity-circuit-input
   :entity-circuit-output-mixin
   :entity-circuit-output
   :entity-conditional-mixin
   :entity-conditional-signal-a
   :entity-conditional-signal-b
   :entity-conditional-operator
   :entity-combinator
   :entity-combinator-operator
   :entity-combinator-signal-a
   :entity-combinator-signal-b
   :entity-combinator-value-p
   :entity-combinator-signal-output
   :entity-combinator-constants
   
   :entity-combinator-decider
   :entity-combinator-arithmetic
   :entity-combinator-constant
   :entity-lamp

   :entity-connect
   :entity-connect-inputs
   :entity-connect-outputs
   :circuit-net-merge
   :circuit-net
   :circuit-inputs
   :circuit-outputs
   :circuit-symbol))

(defpackage :factorio-circuits.graphviz
  (:use :cl)
  (:use :factorio-circuits.entity)
  (:export :entity-graphviz
           :factorio-graphviz))

(defpackage :factorio-circuits.netlist-dsl
  (:use :cl)
  (:use :factorio-circuits.entity)
  (:export :netlist-dsl))

(defpackage :factorio-circuits
  (:use :cl)
  (:use :factorio-circuits.app-utils
   :factorio-circuits.graphviz
   :factorio-circuits.netlist-dsl)
  (:export :-main))
