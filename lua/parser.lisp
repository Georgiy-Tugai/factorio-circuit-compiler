(in-package :lua-parser)

(defvar *symbol-table* (make-instance 'lua-symbol-table))
(defvar *label-table* (make-instance 'lua-symbol-table))

(defrule ws
    (or #\Space #\Tab #\Newline
        (and "--[" (* (and (! "--]") character)) "--]")
        (and "--" (* (and (! #\Newline) character)) #\Newline))
  (:constant nil))

(defrule lua-name
    (and (or (character-ranges (#\A #\Z) (#\a #\z) #\_))
         (* (or (character-ranges (#\A #\Z)
                                 (#\a #\z)
                                 (#\0 #\9)
                                  #\_))))
  (:destructure (first rest)
                (concatenate 'string (string first) rest)))

(defrule lua-unop
    (or #\- "not" #\#))

(defrule lua-binop
    (or ".." "<=" "==" ">=" "and" "or" "~=" #\% #\* #\+ #\- #\/ #\< #\> #\^))

(defrule lua-fieldsep
    (or #\, #\;))

(defrule lua-field
    (or (and #\[ lua-exp #\] #\= lua-exp)
        (and lua-name #\= lua-exp)
        (and lua-exp))
  (:function (lambda (x)
               (case (length x)
                 (5 (list (second x) (fifth x)))
                 (3 (list (first x) (third x)))
                 (1 x)))))

(defrule lua-fieldlist
    (and lua-field (* (and (* ws)
                           lua-fieldsep
                           (* ws)
                           lua-field))
         (? lua-fieldsep))
  (:destructure (first rest _) (declare (ignore _))
                (append (list first)
                        (mapcar #'fourth rest))))

(defrule lua-tableconstructor
    (and #\{ (* ws) (? lua-fieldlist) (* ws) #\})
  (:destructure (_ _2 fields _3 _4) (declare (ignore _ _2 _3 _4))
                (list 'lua-table-constructor
                      (append '(list)
                        (loop for f in fields
                              with n = 0
                              collect
                              (list 'cons
                                    (if (cdr f) (first f) (incf n))
                                    (if (cdr f) (second f) (first f))))))))

(defrule lua-parlist
    (or (and lua-namelist (? (and #\, "...")))
        "..."))

(defrule lua-funcbody
    (and #\( (? lua-parlist) #\) lua-block))

(defrule lua-functiondef
    (and "function" (* ws) lua-funcbody "end"))

(defrule lua-args
    (or (and #\( (? lua-explist) #\))
        lua-tableconstructor
        lua-string)
  (:function (lambda (x)
               (if (and (listp x)
                        (string= (first x) "("))
                   (second x)
                   x))))

(defrule lua-functioncall
    (or (and lua-prefixexp lua-args)
        (and lua-prefixexp #\: lua-name lua-args))
  (:destructure (prefix &rest rest)
                (if (= (length rest) 1)
                    (append (list 'funcall (lua-symbol-find *symbol-table*
                                                            prefix
                                                            :fallback :global))
                            (first rest))
                    (list prefix rest))))

(defrule lua-prefixexp
    (or lua-var
        lua-functioncall
        (and #\( lua-exp #\))))

(defrule lua-number
    (or
     (and "0x"
          (* (character-ranges (#\0 #\9) (#\a #\f) (#\A #\F)))
          (? (and "." (* (character-ranges (#\0 #\9) (#\a #\f) (#\A #\F))))))
     (and (* (character-ranges (#\0 #\9)))
          (? (and "." (* (character-ranges (#\0 #\9)))))
          (? (and "e" (* (character-ranges (#\0 #\9)))))))
  (:destructure (a b c)
                (setf a (concatenate 'string a))
                (read-from-string
                 (concatenate 'string
                              (if (string= a "0x") "#x" "")
                              (if (string= a "0x") b a)
                              (if (if (string= a "0x") c b)
                                  (concatenate 'string "." (second (if (string= a "0x") c b))) "")
                              (if (and (not (string= a "0x")) c)
                                  (concatenate 'string "d" (second c)) ""))
                 nil)))

(defrule lua-string
    (or (and #\"
             (* (or "\\a" "\\b" "\\f" "\\n" "\\r" "\\t" "\\v" "\\\\" "\\\"" "\\'" "\\
" "\\z"
                    (and #\\ (or (and #\x
                                      (character-ranges (#\0 #\9)
                                                        (#\a #\f)
                                                        (#\A #\F))
                                      (character-ranges (#\0 #\9)
                                                        (#\a #\f)
                                                        (#\A #\F)))
                                 (and (character-ranges (#\0 #\9))
                                      (character-ranges (#\0 #\9))
                                      (? (character-ranges (#\0 #\9))))))
                    (and (! #\") character)))
             #\")
        (and #\'
             (* (or "\\a" "\\b" "\\f" "\\n" "\\r" "\\t" "\\v" "\\\\" "\\\"" "\\'" "\\
" "\\z"
                    (and #\\ (or (and #\x
                                      (character-ranges (#\0 #\9)
                                                        (#\a #\f)
                                                        (#\A #\F))
                                      (character-ranges (#\0 #\9)
                                                        (#\a #\f)
                                                        (#\A #\F)))
                                 (and (character-ranges (#\0 #\9))
                                      (character-ranges (#\0 #\9))
                                      (? (character-ranges (#\0 #\9))))))
                    (and (! #\') character)))
             #\'))
  (:function (lambda (res)
               (apply #'concatenate 'string
                      (loop for x in (second res)
                            collect
                            (cond ((stringp x)
                                   (string (code-char (case (char x 1)
                                                        (#\a 7)
                                                        (#\b 8)
                                                        (#\f 12)
                                                        (#\n 10)
                                                        (#\r 13)
                                                        (#\t 9)
                                                        (#\v 118)
                                                        (#\\ 92)
                                                        (#\" 34)
                                                        (#\' 39)
                                                        (#\Newline 10)
                                                        (#\z (error "\\z is not yet supported."))
                                                        (t 0)))))
                                  ((not (first x))
                                   (string (second x)))
                                  ((string= (first x) "\\")
                                   (string
                                    (code-char
                                     (if (string= (first (second x)) "x")
                                         (parse-integer (concatenate 'string (cdr (second x)))
                                                        :radix 16)
                                         (parse-integer (concatenate 'string (second x)))))))
                                  )))))
  )

(defrule lua-exp-unop
    (and lua-unop lua-exp)
  (:destructure (unop exp)
                (list (cond ((equal unop "-")
                             'lua-unm))
                      exp)))

(defrule lua-name-exp
    lua-name
  (:function (lambda (s)
               (lua-symbol-find *symbol-table* s
                                :fallback :global))))

(defrule lua-exp
    (or "nil"
        "false"
        "true"
        "..."
        lua-string
        lua-tableconstructor
        lua-functiondef
        lua-name-exp
        lua-exp-unop
        lua-number
        lua-prefixexp
        (and lua-exp lua-binop lua-exp)))

(defrule lua-explist
    (and lua-exp (* (and (* ws) #\, (* ws) lua-exp)))
  (:destructure (e l)
                (append (list e)
                        (mapcar (lambda (x)
                                  (fourth x))
                                l))))

(defrule lua-namelist
    (and lua-name (* (and #\, (* ws) lua-name)))
  (:destructure (first rest)
                (append (list first) rest)))

(defrule lua-var
    (or lua-name
        (and lua-prefixexp #\[ lua-exp #\])
        (and lua-prefixexp #\. lua-name)))

(defrule lua-varlist
    (and lua-var (* (and #\, (* ws) lua-var))))

(defrule lua-funcname
    (and lua-name (* (and #\. lua-name)) (? (and #\: lua-name))))

(defrule lua-label
    (and "::" lua-name "::")
  (:destructure (_ n _2) (declare (ignore _ _2))
                (lua-symbol-new *label-table* n)))

(defrule lua-retstat
    (and "return" (* ws) lua-explist (? #\;))
  (:destructure (s _ l _2) (declare (ignore s _ _2))
                (list 'return-from 'function (append '(values) l))))

(defrule lua-break
    "break")

(defrule lua-goto
    (and "goto" lua-name)
  (:destructure (_ n) (declare (ignore _))
                (list 'go (lua-symbol-find *label-table* n))))

(defrule lua-do
    (and "do" lua-block "end")
  (:function second))

(defrule lua-while
    (and "while" lua-exp "do" lua-block "end"))

(defrule lua-repeat
    (and "repeat" lua-block "until" lua-exp))

(defrule lua-if
    (and "if" lua-exp "then" lua-block
         (* (and "elseif" lua-exp "then" lua-block))
         (? (and "else" lua-block))
         "end"))

(defrule lua-for
    (and "for"
         (or (and lua-name #\= lua-exp #\, lua-exp (? (and #\, lua-exp)))
             (and lua-namelist "in" lua-explist))
         "do" lua-block "end"))

(defrule lua-function
    (and (? "local ") "function" lua-funcname lua-funcbody))

;; TODO: proper handling of multiple-value expansions!
(defrule lua-local
    (and "local" (* ws) lua-namelist (? (and #\= lua-explist)))
  (:destructure (kw _ names exps) (declare (ignore kw _))
                (loop for n in names and e in (second exps)
                      append (list 'setf (lua-symbol-new *symbol-table*
                                                         n)
                                   e))))

(defrule lua-stat
    (or #\;
        (and lua-varlist (* ws) #\= (* ws) lua-explist)
        lua-functioncall
        lua-label
        lua-break
        lua-goto
        lua-do
        lua-while
        lua-repeat
        lua-if
        lua-for
        lua-function
        lua-local))

(defrule lua-block
    (and (* ws)
         (* (and (! lua-retstat) lua-stat (+ (or #\; ws))))
         (? lua-retstat)
         (* ws))
  (:around ()
           (let ((*symbol-table* (lua-symbol-subtable *symbol-table*))
                 (*label-table* (lua-symbol-subtable *label-table*)))
             (call-transform)))
  (:function (lambda (x)
               (let ((body (list (append '(tagbody)
                                         (append (remove ";" (mapcar #'first (second x)) :test 'equal)
                                                 (when (third x)
                                                   (list (third x)))))))
                     (locals (lua-symbol-list *symbol-table*)))
                 (if (> (hash-table-count locals) 0)
                     (list 'block 'function
                           (append (list 'let
                                         (loop for k being the hash-keys in locals
                                                 using (hash-value v)
                                               collect
                                               (anaphora:aif (lua-symbol-find
                                                              (lua-symbol-parent *symbol-table*) k)
                                                             (list v anaphora:it)
                                                             (list v))))
                                   body))
                     (append '(block function)
                             body))))))

(defrule lua-chunk
    lua-block)
