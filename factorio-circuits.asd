;;;; factorio-circuits.asd

(asdf:defsystem #:factorio-circuits
  :description "Various tools for compiling specifications into Factorio circuit network system blueprints."
  :author "Georgiy Tugai <georgiy.tugai@gmail.com>"
  :license "Modified BSD License"
  :serial t
  :depends-on (:esrap
               :screamer)
  :pathname "./"
  :components ((:file "package")
               (:file "app-utils")
               (:file "factorio-circuits")))

