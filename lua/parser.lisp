(in-package :lua-parser)

(defvar *symbol-table* (make-instance 'lua-symbol-table))
(defvar *label-table* (make-instance 'lua-symbol-table))

(defun lex (input)
  (let ((lexer (make-lua-lexer input)))
    (loop for tok = (step-lua-lexer lexer) while (car tok)
          collect tok)))

(defclass lua-parser ()
  ((lexer :type lua-lexer :initarg :lexer)
   (lex-buffer :type list :initform nil)))

(defgeneric lexer-peek (parser))
(defgeneric lexer-advance (parser))

(defmethod lexer-peek ((parser lua-parser))
  (with-slots (lexer lex-buffer) parser
      (if lex-buffer
          (first lex-buffer)
          (car (push (step-lua-lexer lexer) lex-buffer)))))

(defmethod lexer-advance ((parser lua-parser))
  (with-slots (lexer lex-buffer) parser
    (if lex-buffer
        (pop lex-buffer)
        (step-lua-lexer lexer))))

(defun parse (input)
  (let ((parser (make-instance 'lua-parser :lexer (make-lua-lexer input))))
    (parse-block parser :toplevel t)))

(defvar *debug-loops* nil)

(defmacro peek-loop ((parser token &key (name "peek-loop")) &body body)
  (alexandria:with-gensyms (last-tok)
    `(loop for ,last-tok = nil then ,token
           for ,token = (lexer-peek ,parser)
           for (class . image) = ,token
           when (eql ,last-tok ,token)
             do (error "Infinite loop in token ~A" ,token)
           when *debug-loops*
             do (format t "~A: ~A~%" ,name ,token)
           ,@body)))

(defun match (parser type &optional p)
  (let ((tok (lexer-peek parser)))
    (and (eql (car tok) type)
         (etypecase p
           (null t)
           (string (string= p (cdr tok)))
           (list (member (cdr tok) p :test #'string=))
           (function (funcall p (cdr tok)))))))

(defun match* (type &optional p) (lambda (parser) (match parser type p)))

(defun parse-block (parser &key toplevel)
  (peek-loop (parser token :name "block") 
    with block = '()
    do
    (cond
      ((or (null class))
       (if toplevel
           (return block)
           (error "Unterminated block")))
      ((match parser :keyword "end")
       (lexer-advance parser)
       (return block))
      (t (push (parse-stmt parser) block)))))

(defun skip-junk (parser)
  (peek-loop (parser token :name "skip-junk")
    (if (member class
                '(:start-long-comment :end-long-comment :long-comment-token
                  :comment :comment-token
                  :whitespace))
        (lexer-advance parser)
        (return))))

(defun parse-stmt (parser)
  (flet ((next ()
           (lexer-advance parser)
           (skip-junk parser)))
    (trivia:match (lexer-peek parser)
      ((cons :keyword "do") (next)
       (parse-block parser))
      ((cons :keyword "local") (next)
       (parse-local-stmt parser))
      ((cons :keyword "return") (next)
       (parse-explist parser)))))

(defun parse-exp (parser)
  (flet ((next ()
           (lexer-advance parser)
           (skip-junk parser)))
    (trivia:match (lexer-peek parser)
      ((cons :decimal-number x) (next) x)
      ((cons :keyword "nil") (next) nil)
      ((cons :keyword "false") (next) lua-false)
      ((cons :keyword "true") (next) t)
      ((cons :token "{")
       (parse-tableconstructor parser)
       (skip-junk parser))
      ((cons :name n) (next)
       n))))

(defun parse-local-stmt (parser)
  (let ((first (lexer-peek parser)))
    (if (and (eql (car first) :keyword)
             (equal (cdr first) "function"))
        ;; Local function
        (progn)
        ;; Local variable(s)
        (progn
          (let ((namelist (parse-namelist parser)))
            (unless (match parser :token "=")
              (return-from parse-local-stmt namelist))
            (lexer-advance parser)
            (skip-junk parser)
            (let ((explist (parse-explist parser))))
            ))
        )))

(defun parse-tableconstructor (parser)
  (unless (match parser :token "{")
    (error "Invalid table constructor!"))
  (lexer-advance parser)
  (parse-fieldlist parser)
  (assert (match parser :token "}"))
  (lexer-advance parser))

(defun parse-field (parser)
  (cond
    ((match parser :token "[")
     (lexer-advance parser)
     (parse-exp parser)
     (assert (match parser :token "]"))
     (lexer-advance parser)
     (assert (match parser :token "=")))
    ((match parser :name)
     (lexer-advance parser)
     (skip-junk parser)
     (assert (match parser :token "="))
     (lexer-advance parser)
     (skip-junk parser)
     (parse-exp parser))))

(defmacro def-parse-list ((name &key (sep-p '(match* :token ","))) &body body)
  (alexandria:once-only (sep-p)
    `(defun ,(intern (concatenate 'string "PARSE-" (symbol-name name))) (parser)
       (peek-loop (parser token :name ,(string-downcase (symbol-name name)))
         with ret = '()
         do
         (let ((item ,@body))
           (if item
               (progn (push item ret)
                      (skip-junk parser)
                      (unless (funcall ,sep-p parser)
                        (return ret))
                      (lexer-advance parser))
               (return ret)))))))

(def-parse-list (namelist)
  (when (match parser :name)
    (cdr (lexer-advance parser))))

(def-parse-list (explist)
  (parse-exp parser))

(def-parse-list (fieldlist)
  (parse-field parser))
