(in-package :lua-yacc-parser)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Source: http://letoverlambda.com/index.cl/guest/chap6.html#sec_2
  (defun |#`-reader| (stream sub-char numarg)
    (declare (ignore sub-char))
    (unless numarg (setq numarg 1))
    (let ((args (loop for i from 1 to numarg
                      collect (alexandria:format-symbol *package* "~A~A" 'a i))))
      `(lambda ,args
         (declare (ignorable ,@args))
         ,(funcall
           (get-macro-character #\`) stream nil))))

  (set-dispatch-macro-character #\# #\` #'|#`-reader|)
  )

(define-parser *expression-parser*
  (:start-symbol chunk)
  (:terminals (:number :name :string
                       :|...|
                       :|..| :|<=| :|>=| :|==| :|~=| :|::|
                       :|-| :|+| :|*| :|/| :|^| :|%|
                       :|<| :|>| :|#| :|{| :|}| :|=|
                       :|,| :|;| :|.| :|[| :|]| :|(| :|)|
                       :|:|

                       :and :break :do :else :elseif :end
                       :false :for :function :goto :if :in
                       :local :nil :not :or :repeat :return
                       :then :true :until :while))
  (:precedence ((:right :^)
                (:nonassoc :|#| :not)
                (:left :* :/ :%)
                (:left :+ :-)
                (:right :..)
                (:left :< :> :<= :>= :~= :==)
                (:left :and)
                (:left :or)))

  (chunk
   (lblock #`(block :block ,a1)))

  (lblock
   (lblock2 #`(tagbody ,@a1)))
  
  (lblock2
   (block-stmts #1`(,@(remove :|;| a1))))

  (retstat
   (:return explist :|;| #3`(return-from :block (values ,@(cdr a2))))
   (:return explist #2`(return-from :block (values ,@(cdr a2)))))

  (block-stmts
   (local-stmt block-stmts
               #2`((,(car a1) ,@(cdr a1) (tagbody ,@(remove :|;| a2)))))
   (stmt block-stmts
         #2`(,a1 ,@a2))
   (retstat)
   nil)
  
  (stmts
   (stmt stmts #2`(,a1 ,@a2))
   (stmt))

  (local-stmt
   (:local namelist := explist
           #4`(multiple-value-bind
                    ,(loop for n in a2 collect (intern (invert-case n) 'lua))
                  ,(loop for e on (cdr a4)
                         when (cdr e)
                           collect (car e) into single
                         else
                           return (if single
                                      `(values-list (list* ,@single
                                                           (multiple-value-list ,(car e))))
                                      (car e)))))
   (:local namelist
           #2`(let ,(loop for n in a2 collect (intern (invert-case n) 'lua)))))
  
  (stmt
   :|;|
   (:do lblock :end #3`(,@a2))
   ;; XXX: Figure out how to handle LOCALs properly!! Will probably need a code-walker.
   ;; Lua multiple values can map to Lisp very nicely;
   
   ;; (identity (values 1 2)) => 1, equivalent to (f())
   
   ;; (setf (values x y) (values 1 2)) equivalent to x, y = 1, 2 and
   ;; handles mismatched lengths properly

   ;; The only thing that needs special handling is x, y = 1, f() --
   ;; need to "prepend" 1 to f's multiple value list.
   
   (:|::| :name :|::| (lambda (a1 a2 a3) (declare (ignore a1 a3)) (intern (invert-case a2) 'lua)))
   (:goto :name #2`(go ,(intern (invert-case a2) 'lua))))

  (namelist
   (:name :|,| namelist #3`(,a1 ,@a3))
   (:name))

  (explist
   (explist2 #`(list ,@a1)))
  (explist2
   (expression :|,| explist2 #3`(,a1 ,@a3))
   (expression))

  (tablecons
   (:{ fieldlist :} #3`(lua-runtime:lua-table-constructor (list ,@a2))))

  (fieldlist
   (field fieldsep fieldlist #3`(,a1 ,@a3))
   (field fieldsep #2`(,a1))
   (field))

  (fieldsep :|,| :|;|)

  (field
   (:[ expression :] := expression #5`(cons ,a2 ,a5))
   (:name := expression #3`(cons ,a1 ,a3))
   (expression #`(cons nil ,a1)))
  
  (expression
   :nil :false :true
   tablecons
   :|...|
   (expression :+   expression #3`(lua-add ,a1 ,a3))
   (expression :-   expression #3`(lua-sub ,a1 ,a3))
   (expression :*   expression #3`(lua-mul ,a1 ,a3))
   (expression :/   expression #3`(lua-div ,a1 ,a3))
   (expression :%   expression #3`(lua-mod ,a1 ,a3))
   (expression :..  expression #3`(lua-concat ,a1 ,a3))
   (expression :<   expression #3`(lua-lt ,a1 ,a3))
   (expression :>   expression #3`(lua-le ,a3 ,a1))
   (expression :<=  expression #3`(lua-le ,a1 ,a3))
   (expression :>=  expression #3`(lua-lt ,a3 ,a1))
   (expression :~=  expression #3`(not (lua-eq ,a1 ,a3)))
   (expression :==  expression #3`(lua-eq ,a1 ,a3))
   (expression :and expression #3`(lua-and ,a1 ,a3))
   (expression :or  expression #3`(lua-or ,a1 ,a3))
   uexpression)

  (uexpression
   (uexpression :^ uexpression #3`(lua-pow ,a1 ,a3))
   (:-   uexpression #2`(lua-unm ,a2))
   (:|#| uexpression #2`(lua-len ,a2))
   (:not uexpression #2`(lua-not ,a2))
   term)
  
  (term
   :number :string
   (:name (lambda (n) (intern (invert-case n) 'lua)))
   (:|(| expression :|)| #3`(identity ,a2))))

(defun parse (str)
  (parse-with-lexer (lua-yacc-lexer::lua-lexer (make-string-input-stream str))
                    *expression-parser*))
