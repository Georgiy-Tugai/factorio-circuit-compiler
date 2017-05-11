(in-package :lua-yacc-parser)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun i2p (a b c)
    "Infix to prefix"
    (list b a c))
  
  (defun k-2-3 (a b c)
    "Second out of three"
    (declare (ignore a c))
    b)

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

;; XXX: Figure out how to handle LOCALs!!
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
   (lblock #`(block block ,a1)))
  
  (lblock
   (stmts retstat #2`(,@(remove :|;| a1) ,a2))
   (stmts #`(,@(remove :|;| a1)))
   retstat
   ())

  (retstat
   (:return explist :|;| #3`(return-from block (values ,@(cdr a2))))
   (:return explist #2`(return-from block (values ,@(cdr a2)))))
  
  (stmts
   (stmt stmts)
   stmt)
  
  (stmt
   :|;|
   (:do lblock :end #3`(progn ,@a2))
   (:local namelist := explist
           #4`(setf ,(intern (first a2) 'lua) ,(first (cdr a4))))
   (:local namelist))

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
   (field fieldsep fieldlist #'listitem)
   (field fieldsep #'listitem)
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
   (expression :+ expression #3`(lua-add ,a1 ,a3))
   (expression :- expression #3`(lua-sub ,a1 ,a3))
   (expression :* expression #3`(lua-mul ,a1 ,a3))
   (expression :/ expression #3`(lua-div ,a1 ,a3))
   (expression :% expression #3`(lua-mod ,a1 ,a3))
   (expression :.. expression #3`(lua-concat ,a1 ,a3))
   (expression :< expression #3`(lua-lt ,a1 ,a3))
   (expression :> expression #3`(lua-le ,a3 ,a1))
   (expression :<= expression #3`(lua-le ,a1 ,a3))
   (expression :>= expression #3`(lua-lt ,a3 ,a1))
   (expression :~= expression #3`(not (lua-eq ,a1 ,a3)))
   (expression :== expression #3`(lua-eq ,a1 ,a3))
   (expression :and expression #3`(lua-and ,a1 ,a3))
   (expression :or expression #3`(lua-or ,a1 ,a3))
   uexpression)

  (uexpression
   (uexpression :^ uexpression #3`(lua-pow ,a1 ,a3))
   (:- uexpression #2`(lua-unm ,a2))
   (:|#| uexpression #2`(lua-len ,a2))
   (:not uexpression #2`(lua-not ,a2))
   term)
  
  (term
   :number :string
   (:name (lambda (n) (intern n 'lua)))
   (:|(| expression :|)| #'k-2-3)))

(defun parse (str)
  (parse-with-lexer (lua-yacc-lexer::lua-lexer (make-string-input-stream str))
                    *expression-parser*))
