(in-package :cl-user)
;; (defvar *print-hashtable* t)
;; (defmethod print-object ((object hash-table) stream)
;;   (if *print-hashtable*
;;       (format stream "{~A}"
;;               (alexandria:hash-table-plist object))
;;       (print-unreadable-object (object stream :type t :identity t))))

(set-pprint-dispatch 'lua-types:lua-table
                     (lambda (str obj)
                       (let ((ht (lua-types:lua-to-lisp obj)))
                         (format str "{~{~<~S = ~S~A~_~@:>~^~}}"
                                 (loop for key being the hash-keys of ht
                                       for value being the hash-values of ht
                                       for i = (hash-table-count ht) then (decf i)
                                       collect (list key value
                                                     (if (= i 1)
                                                         "" ", ")))))))
