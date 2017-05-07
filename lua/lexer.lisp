(in-package :lua-lexer)
(cl-interpol:enable-interpol-syntax)

(defmacro define-lexer-class (name fields)
  `(prog1 (defclass ,name (lexer-input-stream)
            ,fields)
     ,@(loop for f in fields collect
             `(defvar ,(intern (concatenate 'string "*" (symbol-name (first f)) "*"))
                ,(anaphora:awhen (getf (cdr f) :initform) anaphora:it)))
     (defmacro ,(intern (concatenate 'string "WITH-" (symbol-name name) "-STATE"))
         (lexer &body body)
       (alexandria:once-only (lexer)
         `(let ,(loop for f in ',fields
                      collect
                      `(,(intern (concatenate 'string "*" (symbol-name (first f)) "*"))
                        (slot-value ,lexer ',(first f))))
            (with-slots ,(loop for f in ',fields collect (first f)) ,lexer
              ,@body))))))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-lexer-class lua-lexer
      ((end-long-comment :initform "")
       (in-long-comment :initform "")

       (in-comment :initform "")

       (end-long-string :initform "")
       (in-long-string :initform "")

       (in-dqstring :initform "")
       (in-sqstring :initform "")
       (backslash :initform "")
       (backslashed :initform ""))))

(defun make-lua-lexer (input)
  (make-instance 'lua-lexer
                 :stream input
                 :rules '((*end-long-comment* . :end-long-comment)
                          (*in-long-comment* . :long-comment-token)
                          (*in-comment* . :comment-token)
                          (*end-long-string* . :end-long-string)
                          (*in-long-string* . :long-string-token)
                          (*backslashed* . :backslashed)
                          (#?r"\n" . :newline)
                          (*backslash* . :backslash)
                          (*in-dqstring* . :dqstring-token)
                          (*in-sqstring* . :sqstring-token)
                          (#?r"--\[+" . :start-long-comment)
                          ("--" . :comment)
                          (#?r"\[=*\[" . :start-long-string)
                          ("\"" . :dquote)
                          ("'" . :squote)
                          (#?r"::[A-Za-z_][A-Za-z_0-9]*::" . :label)
                          (#?r"\.{3}" . :varargs)
                          (#?r"\.\.|<=|>=|==|~=|[-+*/^%<>#]" . :operator)
                          (#?r"\b(?:and|break|do|else|elseif|end|false|for|function|goto|if|in|local|nil|not|or|repeat|return|then|true|until|while)\b" . :keyword)
                          (#?r"0x[A-Fa-f0-9]*(?:\.[A-Fa-f0-9]*)?(?:[pP][+-]?[0-9]+)?" . :hex-number)
                          (#?r"[0-9]*(?:\.[0-9]*)?(?:[eE][+-]?[0-9]+)?" . :decimal-number)
                          ("[A-Za-z_][A-Za-z_0-9]*" . :name)
                          (#?r"\s+" . :whitespace)
                          (#?r"[{}=,;.\[\]()]" . :token)
                          ("." . :unknown))))

(defun step-lua-lexer (lexer)
  (with-lua-lexer-state lexer
    (multiple-value-bind (class image) (stream-read-token lexer)
      (declare ((or null keyword) class)
               ((or null string) image))
      (setf backslashed "")
      (case class
        (:start-long-comment
         (setf end-long-comment
               (concatenate 'string #?r"\]{" (write-to-string (- (length image) 2)) "}--")
               in-long-comment #?r"[^]]+|\]"))
        (:end-long-comment
         (setf end-long-comment ""))
        (:start-long-string
         (setf end-long-string
               (concatenate 'string #?r"\]={" (write-to-string (- (length image) 2)) #?r"}\]")
               in-long-string #?r"[^]]+|\]"))
        (:end-long-string
         (setf end-long-string ""))
        (:comment
         (setf in-comment ".*"))
        (:dquote
         (if (string= in-dqstring "")
             (setf in-dqstring #?r"[^\"\\]+"
                   backslash #?r"\\"
                   class :start-dqstring)
             (setf in-dqstring ""
                   backslash ""
                   class :end-dqstring)))
        (:squote
         (if (string= in-sqstring "")
             (setf in-sqstring #?r"[^'\\]+"
                   backslash #?r"\\"
                   class :start-sqstring)
             (setf in-sqstring ""
                   backslash ""
                   class :end-sqstring)))
        (:backslash
         (setf backslashed #?r"x[0-9a-fA-F]{2}|[0-9]{2,3}|.|\n"))
        (:backslashed
         (if (string= image "z")
             (setf backslashed #?r"[ \t\n]*")
             (setf backslashed "")))
        (:newline
         (unless (string= in-dqstring "")
           (error "Unterminated double-quoted string at ~A:~A"
                  (lexer-row lexer) (lexer-column lexer)))
         (unless (string= in-sqstring "")
           (error "Unterminated single-quoted string at ~A:~A"
                  (lexer-row lexer) (lexer-column lexer)))
         (setf in-comment ""
               in-sqstring ""
               in-dqstring "")))
      (cons class image))))

(defun lua-lex (input)
  (let ((lexer (make-lua-lexer (make-string-input-stream input))))
    (loop for tok = (step-lua-lexer lexer) while (car tok)
          collect tok)))

(defun parse-number (token)
  (let ((radix (ecase (car token)
                 (:decimal-number 10)
                 (:hex-number 16)))
        (string (cdr token)))
    (flet ((.digit-char-p ()
             (.is (lambda (x)
                    (digit-char-p x radix)))))
      (caar
       (run
        (.prog1 (.let* ((_ (case radix
                             (10 (.identity nil))
                             (16 (.string= "0x"))))
                        (integer (.map 'string (.digit-char-p) :at-least 0))
                        (decimal (.optional
                                  (.progn (.char= #\.)
                                          (.map 'string (.digit-char-p) :at-least 0))))
                        (scientific (.optional (.progn (.char= (case radix
                                                                 (10 #\e)
                                                                 (16 #\p)))
                                                       (.map 'string (.digit-char-p))))))
                  (.identity
                   (coerce (* (+ (loop for n across (or (reverse integer) "0")
                                       for i = 1 then (* radix i)
                                       sum (* i (digit-char-p n radix)))
                                 (loop for n across (or decimal "")
                                       for i = radix then (* radix i)
                                       sum (/ (digit-char-p n radix) i)))
                              (expt (ecase radix
                                      (10 10)
                                      (16 2))
                                    (loop for n across (or (reverse scientific) "0")
                                          for i = 1 then (* radix i)
                                          sum (* i (digit-char-p n radix)))))
                           'double-float)))
                (.not (.item)))
        string)))))

(defun parse-string (tokens)
  (do ((tok tokens (cdr tok))
       (ret nil))
      ((null tok) (apply #'concatenate 'string (reverse ret)))
    (ecase (caar tok)
      ((:sqstring-token :dqstring-token) (push (cdar tok) ret))
      (:backslashed
       (cond
         ((string= (cdar tok) "z"))
         ((member (cdar tok) '("\\" "\"" "'" #?"\n") :test #'string=) (push (cdar tok) ret))
         ((member (cdar tok) '("a" "b" "f" "n" "r" "t" "v") :test #'string=)
          (push (cl-interpol:interpol-reader (make-string-input-stream
                                              (concatenate 'string "\"\\" (cdar tok) "\""))
                                             nil nil :recursive-p nil)
                ret))
         (t (error "Unknown Lua string escape: \\~A" (cdar tok)))
         )))))
