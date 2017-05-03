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
                          (#?r"\b(?:and|break|do|else|elseif|end|false|for|function|goto|if|in|local|nil|not|or|repeat|return|then|true|until|while)\b" . :keyword)
                          (#?r"0x[A-Fa-f0-9]*(?:\.[A-Fa-f0-9]*)?(?:[pP][+-]?[0-9]+)?" . :hex-number)
                          (#?r"[0-9]*(?:\.[0-9]*)?(?:[eE][+-]?[0-9]+)?" . :decimal-number)
                          ("[A-Za-z_][A-Za-z_0-9]*" . :name)
                          (#?r"\s+" . :whitespace)
                          (#?r"[-+*/^%<>#]|\.\.|<=|>=|==|~=" . :operator)
                          (#?r"[{}=,;.\[\]]" . :token)
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
