(in-package :lua-parser)
(declaim (optimize (speed 3) (debug 0) (safety 0)))
(cl-interpol:enable-interpol-syntax)

(defvar *symbol-table* (make-instance 'lua-symbol-table))
(defvar *label-table* (make-instance 'lua-symbol-table))

(defmethod input-empty-p ((input cons)) nil)
(defmethod input-empty-p ((input null)) t)
(defmethod input-first ((input cons)) (car input))
(defmethod input-rest ((input cons)) (cdr input))

(defmacro defrule (name args &body body)
  (if (and nil (null args))
      (let ((var (alexandria:symbolicate "*" name "*")))
        `(progn (defparameter ,var
                (progn ,@body))
              (defun ,name () ,var)))
      `(defun ,name ,args ,@body)))

(defrule .lexeme (type &optional (value nil))
  (.is (lambda (l)
         (declare ((cons symbol simple-base-string) l)
                  ((or simple-base-string list) value))
         (and (consp l)
              (etypecase type
                (symbol (eql (car l) type))
                (cons (member (car l) type)))
              (etypecase value
                (string (string= (cdr l) value))
                (cons (member (cdr l) value :test #'string=))
                (null t))))))

(defun .skip (parser &key (at-least 1))
  (.first (.map nil parser :at-least at-least)))

(defrule .junk1 ()
  (.lexeme '(:whitespace :newline
              :comment :comment-token
              :start-long-comment :long-comment-token :end-long-comment)))

(defrule .junk () (.skip (.junk1)))

(defrule .junk? () (.optional (.junk)))

(defun .delim-list (delim item)
  (.concatenate 'list (.mapcar-lazy
                       (.prog1 item delim))
                (.bind item (lambda (l) (.identity (list l))))))

(defun .or-error (parser &key name (message "Parser ~A failed, remaining input ~%~S"))
  (lambda (input)
    (let ((ret (run parser input)))
      (if ret
          ret
          (cerror "Continue"
                  message (or name parser)
                  (apply #'concatenate 'string (mapcar #'cdr input)))))))

(defun .trace (parser &key name fail-only)
  (lambda (input)
    (let ((ret (run parser input)))
      (declare ((or null cons) ret))
      (when (or (not fail-only)
                (not ret))
        (format t "Parser ~A ~A, on input ~S with ~A results~%"
                (or name parser)
                (if ret "succeeded" "failed")
                (apply #'concatenate 'string (loop for i cons in input
                                                   for n fixnum = 0 then (1+ n)
                                                   while (< n 16)
                                                   collect
                                                   (cdr i)))
                (length ret)))
      ret)))

(defrule .chunk ()
  (.let* ((body (.first (.prog2 (.junk?) (.block) (.not (.item))))))
    (.identity
     (list 'block 'block
           body))))

(defrule .block ()
  (lambda (&rest args)
    (let ((*symbol-table* (lua-symbol-subtable *symbol-table*)))
      (apply #'run
             (.prog2 (.skip (.or (.lexeme :token ";")
                                 (.junk1)) :at-least 0)
                     (.let* ((body (.first
                                    (.map 'list
                                          (.prog1 (.stat)
                                                  (.or (.skip (.or (.lexeme :token ";")
                                                                   (.junk1)))
                                                       (.not (.item))))
                                          :at-least 0)))
                             (retstat (.optional
                                       (.let* ((val (.progn (.lexeme :keyword "return")
                                                            (.junk)
                                                            (.delim-list (.lexeme :token ",")
                                                                         (.exp)))))
                                         (.identity
                                          (list (list 'return-from 'block (append '(values) val))))))))
                       (.identity
                        (let ((body (if (> (hash-table-count (lua-symbol-list *label-table*)) 0)
                                        (append '(tagbody) body retstat)
                                        (if (or retstat (> (length (the cons body)) 1))
                                            (append '(progn) body retstat)
                                            (first body)))))
                          (if (> (hash-table-count (lua-symbol-list *symbol-table*)) 0)
                              (list 'let
                                    (loop for sym being
                                            the hash-values of (lua-symbol-list *symbol-table*)
                                          collect sym)
                                    body)
                              body))))
                     (.skip (.or (.lexeme :token ";")
                                 (.junk1)) :at-least 0))
             args))))

(defrule .stat ()
  (.or
   ;; do
   (.and (.let* ((_ (.lexeme :keyword "do"))
                 (_ (.junk))
                 (body (.block))
                 (_ (.lexeme :keyword "end")))
           (.identity body)))
   ;; local namelist = explist
   (.and (.let* ((_ (.lexeme :keyword "local"))
                 (_ (.junk))
                 (namelist (.delim-list (.and (.lexeme :token ",")
                                              (.junk?))
                                        (.lexeme :name)))
                 (explist (.optional (.and (.junk?)
                                           (.lexeme :token "=")
                                           (.junk?)
                                           (.delim-list (.and (.lexeme :token ",")
                                                              (.junk?))
                                                        (.exp))))))
           (.identity
            (append '(setq)
                    (do ((n namelist (cdr n))
                         (e explist (when e (cdr e)))
                         (r nil))
                        ((null n) (nreverse r))
                      (push (lua-symbol-new *symbol-table* (cdar n)) r)
                      (push (first e) r))))))))

(defrule .mapcar-lazy (parser)
  (.plus (.identity ())
         (.let* ((x parser)
                 (xs (.mapcar-lazy parser)))
           (.identity (cons x xs)))))

(defrule .mapcar-lazy-items ()
  (lambda (input)
    (loop for i = input then (input-rest i)
          while (not (input-empty-p i))
          collect (input-first i) into ret
          summing 1 into length fixnum
          finally
             (return (loop for i from 1 upto length
                           collect
                           (loop for c in ret
                                 for i2 = 0 then (1+ i2)
                                 when (< i2 i)
                                   collect c into left
                                 when (>= i2 i)
                                   collect c into right
                                 finally (return (cons left right))))))))

(defrule .mapcar-items ()
  (lambda (input)
    (loop for i = input then (input-rest i)
          while (not (input-empty-p i))
          collect (input-first i) into ret
          summing 1 into length fixnum
          finally
             (return (loop for i from length downto 1
                           collect
                           (loop for c in ret
                                 for i2 = 0 then (1+ i2)
                                 when (< i2 i)
                                   collect c into left
                                 when (>= i2 i)
                                   collect c into right
                                 finally (return (cons left right))))))))

(defun .lit (parser value)
  (.bind parser (lambda (_) (declare (ignore _)) (.identity value))))
(defun .string (start token end)
  (.let* ((_ (.lexeme start))
          (c (.first (.map 'list (.or (.lexeme token)
                                      (.progn (.lexeme :backslash)
                                              (.lexeme :backslashed "z")
                                              (.bind (.lexeme :backslashed)
                                                     (lambda (l)
                                                       (.identity (cons token
                                                                        (cdr l))))))
                                      (.progn (.lexeme :backslash)
                                              (.lexeme :backslashed))))))
          (_ (.lexeme end)))
    (.identity
     (parse-string c))))
(defun .unop (op function)
  (.let* ((_ (.lexeme (if (consp op) (car op) :operator)
                      (if (consp op) (cdr op) op)))
          (_ (.junk?))
          (val (.or (.binop '(("^" lua-pow)) :assoc :right)
                    (.explit))))
    (.identity (list function val))))
(defun .binop (ops &key (assoc :left))
  (declare (list ops))
  (.first (.let* ((a (if (eql assoc :left)
                         (.mapcar-items)
                         (.mapcar-lazy-items)))
                  (_ (.junk?))
                  (op (apply #'.or
                             (mapcar (lambda (op)
                                       (setq op (first op))
                                       (.lexeme (if (consp op) (car op) :operator)
                                                (if (consp op) (cdr op) op)))
                                     ops)))
                  (_ (.junk?))
                  (b (.exp)))
            (let* ((a* (caar (run (.prog1 (.exp)
                                          (.junk?)
                                          (.not (.item)))
                                  a)))
                   (obj (find-if (lambda (x)
                                   (declare ((or simple-base-string (cons symbol string)) x))
                                   (if (stringp x)
                                       (string= x (the simple-base-string (cdr op)))
                                       (equal x op)))
                                 ops :key #'car)))
              (destructuring-bind (_ function &key swap neg) obj
                (declare (ignore _))
                (let ((ret (list
                            function
                            (if swap b a*)
                            (if swap a* b))))
                  ;;(format t "~A: a=~S~%~A: a*=~S~%~A: b=~S~%" op a op a* op b)
                  (if (and a* b)
                      (.identity (if neg
                                     (list 'lua-not ret)
                                     ret))
                      (.fail))))))))

(defrule .explit ()
  (.or
   ;; (.binop '(((:keyword . "or") lua-or)))
   ;; (.binop '(((:keyword . "and") lua-and)))
   ;; (.binop '(("<" lua-lt)
   ;;           (">" lua-le :swap t)
   ;;           ("<=" lua-le)
   ;;           (">=" lua-lt :swap t)
   ;;           ("~=" lua-eq :neg t)
   ;;           ("==" lua-eq)))
   ;; (.binop '((".." lua-concat)) :assoc :right)
   ;; (.binop '(("+" lua-add)
   ;;           ("-" lua-sub)))
   ;; (.binop '(("*" lua-mul)
   ;;           ("/" lua-div)
   ;;           ("%" lua-mod)))
   (.let* ((l (.lexeme :keyword '("nil" "false" "true"))))
     (.identity (cond ((string= (cdr l) "nil") lua-nil)
                      ((string= (cdr l) "false") lua-false)
                      ((string= (cdr l) "true") t))))
   (.let* ((n (.lexeme :decimal-number)))
     (.identity (parse-number n)))
   (.string :start-dqstring :dqstring-token :end-dqstring)
   (.string :start-sqstring :sqstring-token :end-sqstring)
   (.prog2 (.lexeme :start-long-string)
           (.let* ((l (.first (.map 'list (.lexeme :long-string-token)))))
             (.identity (apply #'concatenate 'string
                               (mapcar #'cdr l))))
           (.lexeme :end-long-string))
   (.let* ((n (.lexeme :name)))
     (.identity (lua-symbol-find *symbol-table* (cdr n) :fallback :global)))
   (.tableconstructor)
   ;; (.unop '(:keyword "not") 'lua-not)
   ;; (.unop "#" 'lua-len)
   (.unop "-" 'lua-unm)
   ;; (.binop '(("^" lua-pow)) :assoc :right)
   (.let* ((_ (.lexeme :token "("))
           (_ (.junk?))
           (e (.or-error (.exp) :name "bracketed-exp"))
           (_ (.junk?))
           (_ (.lexeme :token ")")))
     (.identity e))))

(defrule .exp ()
  (.let* ((e (.exp2)))
    (.identity
     (progn (format t "~S~%" e)
            (if (not (cdr e))
                (car e)
                (list :exp e))))))

(defrule .exp2 ()
  (.let* ((lit (.explit))
          (op* (.optional
                (.let* ((op (.or (.lexeme :keyword '("or" "and"))
                                 (.lexeme :operator '("<" ">" "<=" ">=" "~=" "==" ".." "+" "-" "*" "/" "%" "^"))))
                        (right (.exp2)))
                  (.identity (cons op right))))))
    (.identity (cons lit op*))))

(defrule .tableconstructor ()
  (.first
   (.prog2 (.and (.lexeme :token "{") (.junk?))
           (.let* ((l (.optional
                       (.delim-list (.and (.lexeme :token '("," ";"))
                                          (.junk?))
                                    (.let* ((key (.optional
                                                  (.prog1 (.first (.or (.let* ((_ (.lexeme :token "["))
                                                                               (_ (.junk?))
                                                                               (r (.or-error (.exp)
                                                                                             :name "table-key-exp"))
                                                                               (_ (.junk?))
                                                                               (_ (.lexeme :token "]")))
                                                                         (.identity r))
                                                                       (.let* ((n (.lexeme :name)))
                                                                         (.identity (cdr n)))))
                                                          (.junk?)
                                                          (.lexeme :token "=")
                                                          (.junk?))))
                                            (value (.first (.exp))))
                                      (.identity (cons key value)))))))
             (.identity
              (list 'lua-table-constructor
                    (append '(list)
                            (mapcar (lambda (x) `(cons ,(car x) ,(cdr x))) l)))))
           (.junk?)
           (.lexeme :token "}"))))

;; (defclass lua-parser ()
;;   ((lexer :type lua-lexer :initarg :lexer)
;;    (lex-buffer :type list :initform nil)))

;; (defgeneric lexer-peek (parser))
;; (defgeneric lexer-advance (parser))

;; (defmethod lexer-peek ((parser lua-parser))
;;   (with-slots (lexer lex-buffer) parser
;;       (if lex-buffer
;;           (first lex-buffer)
;;           (car (push (step-lua-lexer lexer) lex-buffer)))))

;; (defmethod lexer-advance ((parser lua-parser))
;;   (with-slots (lexer lex-buffer) parser
;;     (if lex-buffer
;;         (pop lex-buffer)
;;         (step-lua-lexer lexer))))
