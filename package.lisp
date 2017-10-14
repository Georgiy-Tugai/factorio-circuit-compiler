(defpackage :factorio-circuits.app-utils
  (:use :cl)
  (:export :internal-disable-debugger)
  (:export :internal-quit))

(defpackage :factorio-circuits
  (:use :cl))

(defpackage :lua-types
  (:use :cl :alexandria :trivial-garbage :anaphora)
  (:export
   :lua-type
   :lua-metatable
   :lua-to-lisp
   :lua-type-name
   :lua-false :lua-nil :lua-boolean :lisp-boolean
   :lua-userdata
   :lua-thread
   :lua-table
   :lua-rawget :lua-rawset))

(defpackage lua-lexer
  (:use :cl :graylex :smug)
  (:export
   :make-lua-lexer
   :step-lua-lexer
   :parse-string
   :parse-number
   :lua-lex))

(defpackage :lua-metatable
  (:use :cl :lua-types :lua-lexer :anaphora)
  (:export :lua-coerce))

(defpackage :lua-runtime
  (:use :cl :lua-types :lua-metatable :anaphora)
  (:export :lua-table-constructor
   :lua-symbol-table :lua-symbol-parent :lua-symbol-find :lua-symbol-subtable :lua-symbol-new :lua-symbol-list
   :lua-or :lua-and :lua-not
   :lua-method-call
   :invert-case
   :lua-numeric-for :lua-iterator-for))

(defpackage :lua-parser
  (:use :cl :lua-metatable :lua-runtime :lua-lexer :lua-types :smug))

(defpackage :lua-preparser
  (:use :cl :lua-metatable :lua-runtime :lua-lexer :lua-types :smug))

(defpackage :lua-yacc-parser
  (:use :cl :lua-metatable :lua-runtime :lua-lexer :lua-types :yacc))

(defpackage :lua-yacc-lexer
  (:use :cl :anaphora))

(defpackage :lua
  (:use))
