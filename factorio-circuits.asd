;;;; factorio-circuits.asd

(asdf:defsystem #:factorio-circuits
  :description "Various tools for compiling specifications into Factorio circuit network system blueprints."
  :author "Georgiy Tugai <georgiy.tugai@gmail.com>"
  :license "Modified BSD License"
  :serial t
  :depends-on (:trivial-garbage
               :green-threads
               :named-readtables
               :graylex
               :cl-interpol
               :anaphora
               :trivia
               :smug :yacc
               :screamer
               :defenum
               :uiop
               :cl-readline)
  :pathname "./"
  :components ((:file "package")
               (:file "app-utils")
               (:file "lua/types")
               (:file "lua/metatable")
               (:file "lua/runtime")
               (:file "lua/lexer")
               ;; (:file "lua/parser")
               (:file "lua/yacc-lexer")
               (:file "lua/yacc")
               (:file "lua/lib/basic")
               (:file "phys/package")
               (:file "phys/prototype")
               (:file "phys/entity")
               (:file "factorio-circuits")))

