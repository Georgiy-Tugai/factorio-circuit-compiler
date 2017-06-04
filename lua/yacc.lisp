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

  (defun lua-intern (str)
    (intern str 'lua))
  (defun lua-intern* (str)
    (lua-intern str))

  (defun walk-update-lexicals (form &optional (lexvars '(lua::|_ENV|)))
    (cond
      ((and (symbolp form)
            (eql (symbol-package form)
                 (find-package 'lua)))
       (if (member form lexvars)
           form
           `(lua-index lua::|_ENV| ,(symbol-name form))))
      ((consp form)
       (case (car form)
         (go)
         (quote)
         (do (let ((lexvars*
                     (append lexvars (mapcar #'car (cadr form)))))
               (setf (cadr form)
                     (mapcar (lambda (x)
                               (walk-update-lexicals x lexvars*))
                             (cadr form)))
               (setf (cdddr form)
                     (mapcar (lambda (x)
                               (walk-update-lexicals x lexvars*))
                             (cdddr form)))))
         (let (let ((lexvars*
                      (append lexvars (mapcar (lambda (x)
                                                (if (consp x) (car x) x))
                                              (cadr form)))))
                (setf (cddr form)
                      (mapcar (lambda (x)
                                (walk-update-lexicals x lexvars*))
                              (cddr form)))))
         (multiple-value-bind
               (walk-update-lexicals lexvars (caddr form))
             (let ((lexvars*
                     (append lexvars (cadr form))))
               (setf (cdddr form)
                     (mapcar (lambda (x) (walk-update-lexicals x lexvars*))
                             (cdddr form)))))
         ;; XXX: Replace this HACK with proper _ENV/_G handling!

         ;; _ENV is always a local -- since _ENV._ENV is nil, this
         ;; proves that local variables are NOT included in _ENV,
         ;; removing the need for hacking around with LET. :D
         ;; (:maybe-defvar
         ;;  (if (member (second form) lexvars)
         ;;      (setf (car form) 'identity
         ;;            (cdr form) nil)
         ;;      (setf (car form) 'defvar)))
         ((setf setq)
          (loop for f on (cdr form)
                for n from 0
                ;;when (= (mod n 2) 1)
                  do (setf (car f)
                           (walk-update-lexicals (car f)
                                                 lexvars))))
         (t (setf (cdr form)
                  (mapcar (lambda (x) (walk-update-lexicals x lexvars))
                          (cdr form)))))
       form)
      (t form))))

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
                (:left :or)
                ))

  (chunk
   (lblock #`(block :block ,(walk-update-lexicals a1))))

  (lblock
   (lblock2 #`(,(if (loop for x in a1 never (symbolp x))
                    'progn 'tagbody)
                 ,@a1)))
  
  (lblock2
   (block-stmts #1`(,@(remove :|;| a1))))

  (retstat
   (:return explist :|;| #3`(return-from :block (values ,@(cdr a2))))
   (:return explist #2`(return-from :block (values ,@(cdr a2)))))

  (block-stmts
   (local-stmt lblock
               #2`((,(car a1) ,@(cdr a1)
                              ,@(cdr a2))))
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
                    ,(loop for n in a2 collect (lua-intern n))
                  ,(loop for e on (cdr a4)
                         when (cdr e)
                           collect (car e) into single
                         else
                           return (if single
                                      `(values-list (list* ,@single
                                                           (multiple-value-list ,(car e))))
                                      (car e)))))
   (:local namelist
           #2`(let ,(loop for n in a2 collect (lua-intern n)))))
  
  (stmt
   :|;|
   (lvallist := explist
             #3`(setf
                 ,(if (or (cdr a1) (cddr a3))
                      `(values ,@a1)
                      (car a1))
                 ,(loop for e on (cdr a3)
                        when (cdr e)
                          collect (car e) into single
                        else
                          return (if (or (cdr a1) single)
                                     `(values-list (list* ,@single
                                                          (multiple-value-list  ,(car e))))
                                     (car e)))))
   functioncall
   (:|::| :name :|::| (lambda (a1 a2 a3) (declare (ignore a1 a3)) (lua-intern a2)))
   (:break #`(loop-finish))
   (:goto :name #2`(go ,(lua-intern a2)))

   (:do lblock :end #3`(,@a2))
   (:while expression :do lblock :end
           #5`(loop while (lisp-boolean ,a2)
                    do ,a4))
   (:repeat lblock :until expression
            #4`(loop do ,a2
                     when (lisp-boolean ,a4)
                       do (loop-finish)))
   (:if expression :then lblock maybe-elseif :end
        #6`(cond
             ((lisp-boolean ,a2) ,a4)
             ,@a5))
   (:for :name := explist :do lblock :end
         #7`(do ((limit (lua-coerce ,(third a4) 'number :must t))
                 (step ,(if (fourth a4)
                            `(lua-coerce ,(fourth a4) 'number :must t)
                            1))
                 (,(lua-intern a2)
                  (lua-coerce ,(second a4) 'number :must t)
                  (+ ,(lua-intern a2) step)))
                ((or (and (> step 0)
                          (>= ,(lua-intern a2)
                              limit))
                     (and (< step 0)
                          (< ,(lua-intern a2))
                          limit))
                 (= step 0))
              ,a6))
   )

  (maybe-elseif
   (:elseif expression :then lblock maybe-elseif
            #5`(((lisp-boolean ,a2) ,a4) ,@a5))
   (:else lblock
          #2`((t ,a2)))
   ())

  (lvallist
   (term :|,| lvallist #3`(,a1 ,@a3))
   (term))
  
  (namelist
   (:name :|,| namelist #3`(,a1 ,@a3))
   (:name))

  (explist
   (explist2 #`(list ,@a1)))
  (explist2
   (expression :|,| explist2 #3`(,a1 ,@a3))
   (expression))

  (tablecons
   (:{ fieldlist :} #3`(lua-runtime:lua-table-constructor (list ,@a2)))
   (:{ :} #2`(lua-runtime:lua-table-constructor (list))))

  (fieldlist
   (field fieldsep fieldlist #3`(,a1 ,@a3))
   (field fieldsep #2`(,a1))
   (field))

  (fieldsep :|,| :|;|)

  (field
   (:[ expression :] := expression #5`(cons ,a2 ,a5))
   (:name := expression #3`(cons ,a1 ,a3))
   (expression #`(cons nil ,a1)))

  (args
   (:|(| explist :|)| #3`(,@(cdr a2)))
   (:|(| :|)| #2`())
   (tablecons)
   (:string))

  (functioncall
   (term args
         #2`(funcall ,a1 ,@a2))

   (functioncall args
         #2`(funcall ,a1 ,@a2))
   
   (term :|:| :name args
         #4`(lua-method-call ,a1 ,a3 ,@a4))

   (functioncall :|:| :name args
         #4`(lua-method-call ,a1 ,a3 ,@a4)))
  
  (expression
   :|...|
   tablecons
   functioncall
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
   term
   trivterm)

  (term
   (:name (lambda (n) (lua-intern* n)))
   (term :[ expression :] #4`(lua-index ,a1 ,a3))
   (term :|.| :name #3`(lua-index ,a1 ,a3))
   (:|(| expression :|)| #3`(identity ,a2)))
  
  (trivterm :number
            :string #'lua-lexer::parse-string
            (:nil #`(,@lua-nil))
            (:false #`(,@lua-false))
            (:true #`(,@t))))

(defun parse (str &key (environment 'lua::|_G|))
  `(let ((lua::|_ENV| ,environment))
     (declare (ignorable lua::|_ENV|))
     ,(parse-with-lexer (lua-yacc-lexer::lua-lexer (make-string-input-stream str))
                        *expression-parser*)))
