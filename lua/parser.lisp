(in-package :lua-parser)

(defvar *symbol-table* (make-instance 'lua-symbol-table))
(defvar *label-table* (make-instance 'lua-symbol-table))

(defun lex (input)
  (let ((lexer (make-lua-lexer (make-string-input-stream input))))
    (loop for tok = (step-lua-lexer lexer) while (car tok)
          collect tok)))

(defmethod input-empty-p ((input cons)) nil)
(defmethod input-empty-p ((input null)) t)
(defmethod input-first ((input cons)) (car input))
(defmethod input-rest ((input cons)) (cdr input))

(defun .lexeme (type &optional (value nil))
  (.is (lambda (l)
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

(defun .junk1 ()
  (.lexeme '(:whitespace
              :comment :comment-token
              :start-long-comment :long-comment-token :end-long-comment)))

(defun .junk () (.skip (.junk1)))

(defun .junk? () (.optional (.junk)))

(defun .delim-list (delim item)
  (.concatenate 'list (.map 'list
                            (.prog1 item delim)
                            :at-least 0)
                (.bind item (lambda (l) (.identity (list l))))))

(defun .chunk ()
  (.let* ((body (.first (.prog2 (.junk?) (.block) (.not (.item))))))
    (.identity
     (list 'block nil
           body))))

(defun .block ()
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
                                          (list (list 'return nil (append '(values) val))))))))
                       (.identity
                        (let ((body (if (> (hash-table-count (lua-symbol-list *label-table*)) 0)
                                        (append '(tagbody) body retstat)
                                        (if (or retstat (> (length body) 1))
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

(defun .stat ()
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

(defun .exp ()
  (flet ((.lit (parser value)
           (.bind parser (lambda (_) (declare (ignore _)) (.identity value))))
         (.string (start token end)
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
              (parse-string c)))))
    (.or (.lit (.lexeme :keyword "nil") lua-nil)
         (.lit (.lexeme :keyword "false") lua-false)
         (.lit (.lexeme :keyword "true") t)
         (.let* ((n (.lexeme :decimal-number)))
           (.identity (read-from-string (cdr n))))
         (.string :start-dqstring :dqstring-token :end-dqstring)
         (.string :start-sqstring :sqstring-token :end-sqstring)
         (.prog2 (.lexeme :start-long-string)
                 (.let* ((l (.first (.map 'list (.lexeme :long-string-token)))))
                   (.identity (apply #'concatenate 'string
                                     (mapcar #'cdr l))))
                 (.lexeme :end-long-string))
         (.let* ((n (.lexeme :name)))
           (.identity (lua-symbol-find *symbol-table* (cdr n) :fallback :global))))))

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
