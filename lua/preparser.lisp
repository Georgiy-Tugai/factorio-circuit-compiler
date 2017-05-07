(in-package :lua-preparser)
(declaim (optimize (speed 3) (debug 0) (safety 0)))
(cl-interpol:enable-interpol-syntax)

(defmacro scase (keyform* &rest cases)
  (let* ((blockname (gensym))
         default
         (keyform (gensym "KEYFORM"))
         (len (gensym "LEN")))
    (labels ((subcases (offset cases)
               (declare (fixnum offset))
               (macrolet ((putcase (key value h)
                            `(if (> (length (the string ,key)) 0)
                                 (push (cons (subseq (the string ,key) 1) ,value)
                                       (gethash (char (the simple-string ,key) 0) ,h))
                                 (push (cons "" ,value)
                                       (gethash 'empty ,h)))))
                 (loop for c of-type (cons (or string cons) cons) in cases
                       with h = (make-hash-table)
                       when (stringp (car c))
                         do (putcase (car c) (cdr c) h)
                       when (consp (car c))
                         do (loop for k2 in (car c)
                                  do (putcase k2 (cdr c) h))
                       when (and (= offset 0)
                                 (eql (car c) t))
                         do (setf default (append default (cdr c)))
                       finally
                          (loop for k being the hash-keys of h
                                  using (hash-value v)
                                do
                                   (setf (gethash k h)
                                         (if (cdr v)
                                             (subcases (1+ offset) v)
                                             `(when (string= ,keyform ,(caar v)
                                                             :start1 ,(if (eql k 'empty)
                                                                          offset
                                                                          (the fixnum
                                                                               (1+ offset))))
                                                (return-from ,blockname
                                                  (progn ,@(cdar v)))))))
                          (return (tocode offset
                                          (alexandria:hash-table-alist h))))))
             (tocode (offset subcases)
               `(case (if (> ,len ,offset)
                          (char ,keyform ,offset)
                          'empty)
                  ,@(loop for (k . v) in subcases
                          collect
                          `(,k ,v)))))
      `(let* ((,keyform ,keyform*)
              (,len (length ,keyform)))
         (block ,blockname
           ,(subcases 0 cases)
           ,@default)))))

(defvar *tcase-default* (lambda () nil))

(defmacro tcase (keyform* &body cases)
  (let* ((keyform (gensym "KEYFORM"))
         default
         (deflabel (gensym "DEFAULT"))
         (matched (gensym "MATCHED"))
         (ret (gensym "RET"))
         (cases (loop for c in cases
                      with h = (make-hash-table)
                      when (eql (car c) t)
                        do (setf default (append default (cdr c)))
                      when (keywordp (car c)) ;all tokens of :type
                        do (push (cons t `((progn (setf ,matched t)
                                                  ,@(cdr c))))
                                 (gethash (car c) h))
                      when (consp (car c))
                        do (if (and (symbolp (caar c))
                                    (typep (cdar c) '(or (cons string) string))) ;one case, or many cases
                               (push (cons (cdar c) `((progn (setf ,matched t)
                                                             ,@(cdr c))))
                                     (gethash (caar c) h))
                               (loop for c2 in (car c)
                                     do (push (cons (if (consp c2)
                                                        (cdr c2) t)
                                                    `((progn (setf ,matched t)
                                                             ,@(cdr c))))
                                              (gethash (car c2) h))))
                      finally
                         (return (loop for k being the hash-keys of h
                                         using (hash-value v)
                                       collect
                                       `(,k ,@(if (and (not (cdr v))
                                                       (not (cddr v))
                                                       (eql (caar v) t))
                                                  (cdar v)
                                                  `((let ((,ret (when (stringp (cdr ,keyform))
                                                                  (scase (cdr ,keyform)
                                                                    ,@v))))
                                                      (if ,matched
                                                          ,ret
                                                          (,deflabel))))))))
                      )))
    `(let ((,keyform ,keyform*)
           (,matched nil))
       ;; XXX: Remove this if you want list-tokens
       (declare ((cons symbol string) ,keyform))
       (flet ((,deflabel ()
                ,@(or default '((funcall *tcase-default*)))))
         (case (car ,keyform)
           ,@cases
           (t (,deflabel)))))))

(defun preparse (tokens)
  (do ((tok (append tokens (list (cons :eof ""))) (cdr tok))
       (stack (list (cons :chunk nil))))
      ((null tok) stack)
    (let ((*tcase-default*
            (lambda ()
              (error "Token ~A invalid in context ~A"
                     (car tok) (mapcar #'car stack)))))
      (macrolet
          ((ret ()
             `(progn
                                        ;(format t "~A ret: ~A~%" (caar stack) (cdar stack))
                (push (let ((s (pop stack)))
                        (cons (car s)
                              (nreverse (cdr s))))
                      (cdar stack))))
           (ret2 ()
             `(let ((s (pop stack)))
                (setf (cdar stack)
                      (append (cdr s) (cdar stack)))))
           (add ()
             `(push (car tok)
                    (cdar stack)))
           (rec (&rest types)
             `(progn
                ,@(loop for typ in types
                        collect `(push (cons ,typ nil)
                                       stack))))
           (exec (type)
             `(setf (caar stack) ,type))
           (rept ()
             `(progn ;(format t "rept: ~A~%" (car tok))
                (setf tok (cons nil tok)))))
        (labels ((body ()
                   (case (caar stack)
                     ((:chunk :block)
                      (tcase (car tok)
                        ((:keyword "do") (rec :block))
                        ((:keyword "end")
                         (ret)
                         (when (member (caar stack) '(:while :if))
                           (ret)))
                        ((:keyword "return") (rec :return :list :exp))
                        ((:keyword "goto") (rec :goto))
                        ((:keyword "while") (rec :while :exp))
                        ((:keyword "repeat") (rec :repeat :block))
                        ((:keyword "until")
                         (if (eql (caadr stack) :repeat)
                             (progn (ret) (rept))
                             (funcall *tcase-default*)))

                        ((:keyword "if") (rec :if :exp))
                        ((:keyword "elseif")
                         (if (eql (caadr stack) :if)
                             (progn (ret) (rept))
                             (funcall *tcase-default*)))
                        ((:keyword "else")
                         (if (eql (caadr stack) :if)
                             (progn (ret) (rept))
                             (funcall *tcase-default*)))

                        ((:keyword "local")
                         (rec :local-kw))
                        
                        ((:token ";"))
                        ((:token "(") (rec :closebracket :exp))
                        (t (add))))

                     (:goto
                      (tcase (car tok)
                        (:name (add) (ret))))

                     (:while
                      (tcase (car tok)
                        ((:keyword "do") (rec :block))))

                     (:repeat
                      (tcase (car tok)
                        ((:keyword "until") (rec :exp))
                        (t (ret) (rept))))

                     (:if
                      (tcase (car tok)
                        ((:keyword "then" "else") (add) (rec :block))
                        ((:keyword "elseif") (add) (rec :exp))))

                     (:local-kw
                      (tcase (car tok)
                        ;;((:keyword "function") (exec :local-function))
                        (:name (exec :local) (rec :list :name) (rept))))

                     (:name
                      (tcase (car tok)
                        (:name (add))
                        (t (ret2) (rept))))
                     
                     (:local
                       (tcase (car tok)
                         ((:token "=") (rec :list :exp))
                         (t (ret) (rept))))
                     
                     (:exp
                      (tcase (car tok)
                        ((:operator
                          :decimal-number :hex-number
                          :varargs :name)
                         (add))
                        ((:keyword "false" "true" "nil") (add))
                        ((:token "." ":") (add))
                        ((:token ",")
                         (if (member (caadr stack) '(:closebracket :exp))
                             (add)
                             (progn (ret) (rept))))
                        ((:token "(") (rec :closebracket :exp))
                        (t (ret) (rept))))

                     (:closebracket
                      (tcase (car tok)
                        ((:token ")") (ret2))))
                     (:closesqbrace
                      (tcase (car tok)
                        ((:token "]") (ret2))))
                     (:closecurlybrace
                      (tcase (car tok)
                        ((:token "}") (ret2))))

                     ((:list :return :table) (ret) (rept))

                     (:dqstring
                      (tcase (car tok)
                        (:dqstring-token (add))
                        (:end-dqstring (ret))))
                     (:sqstring
                      (tcase (car tok)
                        (:sqstring-token (add))
                        (:end-sqstring (ret))))
                     
                     (:table-item
                      (tcase (car tok)
                        ((:token "(") (rec :exp))
                        ((:token "[") (rec :closesqbrace :exp))
                        ((:token "=") (rec :exp))
                        ((:token "}") (ret) (rept))
                        (t (add))))

                     (t (add)))))
          ;; (let ((*print-length* 3))
          ;;   (format t "~S~%~S~%~%"
          ;;           stack
          ;;           (car tok)))
          (tcase (car tok)
            ((:whitespace :newline
              :comment :comment-token
              :start-long-comment :long-comment-token :end-long-comment))
            ((:token ",")
             (if (member (caadr stack) '(:list :table))
                 (progn
                   (push (cons (caar stack) (nreverse (cdar stack)))
                         (cdadr stack))
                   (setf (cdar stack) nil))
                 (body)))
            ((:token ";")
             (if (member (caadr stack) '(:table))
                 (progn
                   (push (cons (caar stack) (nreverse (cdar stack)))
                         (cdadr stack))
                   (setf (cdar stack) nil))
                 (body)))
            (:start-dqstring (rec :dqstring))
            (:start-sqstring (rec :sqstring))
            (:eof
             (if (eql (caar stack) :chunk)
                 (setf (cdar stack)
                       (reverse (cdar stack)))
                 (body)))
            ((:token "{")
             (rec :closecurlybrace :table :table-item))
            (t (body))))))))
