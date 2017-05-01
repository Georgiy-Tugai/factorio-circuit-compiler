(in-package :cl-user)
;; (defvar *print-hashtable* t)
;; (defmethod print-object ((object hash-table) stream)
;;   (if *print-hashtable*
;;       (format stream "{~A}"
;;               (alexandria:hash-table-plist object))
;;       (print-unreadable-object (object stream :type t :identity t))))

(set-pprint-dispatch 'hash-table
                     (lambda (str ht)
                       (format str "{ ~{~<~A = ~S ~_~:>~}}"
                               (loop for key being the hash-keys of ht
                                     for value being the hash-values of ht
                                     collect (list key value)))))
