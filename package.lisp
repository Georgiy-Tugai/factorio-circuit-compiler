(defpackage :factorio-circuits.app-utils
  (:use :cl)
  (:export :internal-disable-debugger)
  (:export :internal-quit))

(defpackage :factorio-circuits
  (:use :cl))

(defpackage :lua-types
  (:use :cl :alexandria :trivial-garbage)
  (:export
   :lua-type
   :lua-metatable
   :lua-to-lisp
   :lua-type-name
   :lua-false :lua-nil :lua-boolean
   :lua-userdata
   :lua-thread
   :lua-table
   :lua-rawget :lua-rawset))

(defpackage :lua-metatable
  (:use :cl :lua-types))

(defpackage :lua-runtime
  (:use :cl :lua-types :lua-metatable)
  (:export :lua-table-constructor
           :lua-symbol-table :lua-symbol-parent :lua-symbol-find :lua-symbol-subtable :lua-symbol-new :lua-symbol-list))

(defpackage lua-lexer
  (:use :cl :graylex)
  (:export make-lua-lexer
           step-lua-lexer))

(defpackage :lua-parser
  (:use :cl :lua-metatable :lua-runtime :esrap))

(defpackage :lua
  (:use))
