(in-package :factorio-circuits)

(defun -main (&optional args)
  (loop
    for in = (cl-readline:readline :prompt "> ")
    until (null in)
    do
    (let (parse
          input-done)
      (setq in (cl-ppcre:regex-replace "^=" in "return "))
      (loop until input-done
            do
               (handler-case
                   (setq parse (lua-yacc-parser:parse in)
                         input-done t)
                 (yacc:yacc-parse-error
                   (e)
                   (if (eql (yacc:yacc-parse-error-terminal e) nil)
                       (progn
                         (format t "Expected: ~{~(~A~)~^, ~}~%" (yacc:yacc-parse-error-expected-terminals e))
                         (let ((in2 (cl-readline:readline :prompt ">> ")))
                           (if in2
                               (setq in (concatenate 'string in " " in2))
                               (progn (setq input-done t)
                                      (format t "~%")))))
                       (progn (format t "~A~%" e)
                              (setq input-done t))))))
      (when parse
        (handler-case
            (apply 'lua-metatable:lua-call (lua-metatable:lua-index lua::|_G| "print")
                   (multiple-value-list (eval parse)))
          (t (e) (format t "~A~%" e)))))))
