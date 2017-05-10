(in-package :lua-yacc-lexer)
(cl-interpol:enable-interpol-syntax)

(defvar *buffer-size* 512)

(defun lua-identifier-char-p (char initial)
  (and (typep char 'character)
       (or (if initial
               (alpha-char-p char)
               (alphanumericp char))
           (eql char #\_))))

(defun lua-lexer (stream)
  (let ((buf
          (make-array *buffer-size*
                      :element-type 'character))
        (offset 0)
        (head 0)
        (stream-position 0)
        (last-token nil))
    (labels ((bread-lbracket ()
               (and (eql (bpeekc) #\[)
                    (let ((eqls (loop for i = 1 then (1+ i)
                                      while (eql (bpeekc i) #\=)
                                      sum 1)))
                      (when (string= (bpeekc (1+ eqls)) #\[)
                        (bread (+ eqls 2))
                        eqls))))
             (l-step ()
               (acond
                 ((string= (bpeek 2) "--")
                  ;; Comment
                  (bread 2)
                  (let ((lbracket (bread-lbracket)))
                    (if lbracket
                        ;; Long comment
                        (let ((lbstr (concatenate 'string "]"
                                                  (make-string lbracket :initial-element #\=)
                                                  "]")))
                          (loop until (string= (bpeek (+ 2 lbracket)) lbstr)
                                do (bread))
                          (loop until (eql (breadc) #\Newline)))
                        ;; Short comment
                        (loop until (eql (breadc) #\Newline)))
                    (setf last-token '(:comment))
                    (l-step)))
                 ((bread-lbracket)
                  ;; Long string
                  (let* ((lbstr (concatenate 'string "]"
                                             (make-string it :initial-element #\=)
                                             "]"))
                         (ret (apply #'concatenate
                                     'string (loop until (string= (bpeek (+ 2 it)) lbstr)
                                                   collect (bread)))))
                    (bread (+ 2 it))
                    (values :string ret)))
                 ((member (bpeekc) '(#\' #\"))
                  ;; Single or double-quoted string
                  (let ((qtype (bread)))
                    (do ((c (bpeek) (bpeek))
                         (r nil))
                        ((string= c qtype)
                         (bread)
                         (values :string (apply #'concatenate 'string (nreverse r))))
                      (when (= (length c) 0)
                        (error "Unterminated ~A-quoted string!"
                               (if (string= qtype "'") "single" "double")))
                      (case (schar c 0)
                        (#\\ (push (bread 2) r))
                        (t (push (bread) r))))
                    ))
                 ((string= (bpeek 3) "...")
                  (bread 3)
                  (values :|...|))
                 ((member (bpeek 2) '(".." "<=" ">=" "==" "~=" "::") :test 'string=)
                  (values (alexandria:make-keyword (bread 2))))
                 ((member (bpeek) '("-" "+" "*" "/" "^" "%"
                                    "<" ">" "#" "{" "}" "="
                                    "," ";" "." "[" "]" "(" ")"
                                    ":") :test 'string=)
                  (values (alexandria:make-keyword (bread))))
                 ((and
                   (not (member (car last-token) '(:keyword :name :number :unknown)))
                   (lua-identifier-char-p (bpeekc) t))
                  (let ((ret (apply #'concatenate 'string
                                    (loop until (not (lua-identifier-char-p (bpeekc) nil))
                                          collect (bread)))))
                    (if (member ret '("and" "break" "do" "else" "elseif" "end"
                                      "false" "for" "function" "goto" "if" "in"
                                      "local" "nil" "not" "or" "repeat" "return"
                                      "then" "true" "until" "while"))
                        (values (alexandria:make-keyword ret))
                        (values :name ret))))
                 ((string= (bpeek 2) "0x")
                  ;; Hexadecimal number
                  (bread 2)
                  (let ((ret))
                    (loop while (digit-char-p (bpeekc) 16)
                          do (push (bread) ret))
                    (when (char= (bpeekc) #\.)
                      (push (bread) ret)
                      (loop while (digit-char-p (bpeekc) 16)
                            do (push (bread) ret)))
                    (when (and (not (cdr ret))
                               (string= (first ret) "."))
                      (error "Malformed hex number"))
                    (when (char-equal (bpeekc) #\p)
                      (push (bread) ret)
                      (when (member (bpeekc) '(#\+ #\-))
                        (push (bread) ret))
                      (loop while (digit-char-p (bpeekc))
                            do (push (bread) ret)))
                    (values :number (lua-lexer::parse-number
                                     (cons :hex-number
                                           (apply #'concatenate
                                                  'string "0x" (nreverse ret)))))))
                 ((or (and (bpeekc) (digit-char-p (bpeekc)))
                      (and (bpeekc 1)
                           (char= (bpeekc) #\.)
                           (digit-char-p (bpeekc 1))))
                  ;; Decimal number
                  (let ((ret))
                    (loop while (digit-char-p (bpeekc))
                          do (push (bread) ret))
                    (when (char= (bpeekc) #\.)
                      (push (bread) ret)
                      (loop while (digit-char-p (bpeekc))
                            do (push (bread) ret)))
                    (when (and (not (cdr ret))
                               (string= (first ret) "."))
                      (error "Malformed number"))
                    (when (char-equal (bpeekc) #\e)
                      (push (bread) ret)
                      (when (member (bpeekc) '(#\+ #\-))
                        (push (bread) ret))
                      (loop while (digit-char-p (bpeekc))
                            do (push (bread) ret)))
                    (values :number (lua-lexer::parse-number
                                     (cons :decimal-number
                                           (apply #'concatenate
                                                  'string (nreverse ret)))))))
                 ((member (bpeekc) '(#\Space #\Tab #\Newline))
                  (setf last-token '(:whitespace))
                  (bread) (l-step))
                 (t (values :unknown (bread)))))
             (flip-buffer ()
               ;; (format t "offset=~A head=~A buf=~S~%" offset head buf)
               (when (> offset 0)
                 ;; (setf (subseq buf 0 (- (min *buffer-size* head) offset))
                 ;;       (subseq buf offset (min *buffer-size* head)))
                 (replace buf buf
                          :start1 0 :end1 (- (min *buffer-size* head) offset)
                          :start2 offset :end2 (min *buffer-size*
                                                    head))
                 (setf head (- head offset))
                 (incf stream-position offset)
                 (setf offset 0))
               ;; (format t "offset=~A head=~A buf=~S~%" offset head buf)
               (setf head
                     (read-sequence buf stream :start head :end *buffer-size*))
               ;; (format t "after-read: offset=~A head=~A buf=~S~%" offset head buf)
               )
             (bpeek (&optional (length 1) (distance 0))
               (assert (<= (+ distance length) *buffer-size*))
               ;; (format t "offset=~A head=~A buf=~S~%" offset head buf)
               (when (>= (+ offset distance length) head)
                 (flip-buffer))
               ;; (when (>= (+ offset distance) head)
               ;;   (error 'end-of-file :stream stream))
               (subseq buf (min head (+ offset distance)) (min head (+ offset distance length))))
             (bpeekc (&optional (distance 0))
               (let ((ret (bpeek 1 distance)))
                 (when (> (length ret) 0)
                   (char ret 0))))
             (bread (&optional (length 1))
               (assert (<= length *buffer-size*))
               ;; (format t "offset=~A head=~A buf=~S~%" offset head buf)
               (when (>= (+ offset length) head)
                 (flip-buffer))
               (when (>= offset head)
                 (error 'end-of-file :stream stream))
               (prog1 (subseq buf (min head offset) (min head (+ offset length)))
                 (setf offset (min head (+ offset length)))))
             (breadc ()
               (let ((ret (bread 1)))
                 (when (> (length ret) 0)
                   (char ret 0)))))
      (flip-buffer)
      (lambda ()
        (handler-case
            (values-list
             (setf last-token
                   (multiple-value-list (l-step))))
          (end-of-file nil))
        ))))
