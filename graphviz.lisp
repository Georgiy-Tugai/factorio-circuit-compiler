(in-package :factorio-circuits.graphviz)

(defgeneric entity-graphviz (port entity &key &allow-other-keys))
(defgeneric entity-graphviz-name (entity))

(defmethod entity-graphviz-name (entity) (format nil "<i>~A</i>"
                                                 (string-downcase (class-name (class-of entity)))))
(defmethod entity-graphviz-name ((entity entity-other))
  (format nil "<i>~A</i>"
          (entity-type entity)))

(defmethod entity-graphviz (port entity &key &allow-other-keys)
  (format port "~A [label=<~A>];~%"
          (entity-symbol entity)
          (entity-graphviz-name entity)))

(defun draw-signal (sig)
  (if (typep sig 'symbol)
      (let ((str (string-downcase (symbol-name sig))))
        (if (equal (subseq str 0 7) "signal-")
            (format nil "[~A]" (subseq str 7))
            str))
      sig))

(defun draw-conditional (port entity &key &allow-other-keys)
  (format port "~{<td border=\"0\">~A</td>~}"
          (list (draw-signal (entity-conditional-signal-a entity))
                (case (entity-conditional-operator entity)
                  (:> "&gt;") (:< "&lt;") (t (entity-conditional-operator entity)))
                (draw-signal (entity-conditional-signal-b entity)))))

(defmethod entity-graphviz (port (entity entity-conditional-mixin) &key &allow-other-keys)
  (if (slot-boundp entity 'entity-conditional-operator)
      (progn
        (format port "~A [label=<<table><tr><td colspan=\"3\" border=\"0\">~A</td></tr><tr>"
                (entity-symbol entity)
                (entity-graphviz-name entity))
        (draw-conditional port entity)
        (format port "</tr></table>>];~%"))
      (call-next-method)))

(defmethod entity-graphviz (port (entity entity-combinator-constant) &key &allow-other-keys)
  (format port "~A [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr>~{~{<td>~A&nbsp;=&nbsp;~A</td>~}~}</tr></table>>];~%"
          (entity-symbol entity)
          (mapcar (lambda (x) (list (draw-signal (car x)) (cdr x)))
                  (entity-combinator-constants entity))))

(defmethod entity-graphviz (port (entity entity-combinator-decider) &key &allow-other-keys)
  (format port "~A [label=<<table><tr>"
          (entity-symbol entity))
  (draw-conditional port entity)
  (format port "~{<td border=\"0\">~A</td>~}</tr></table>>];~%"
          (list (concatenate
                 'string "&rArr;"
                 (if (eql (entity-combinator-signal-output entity)
                          (entity-conditional-signal-a entity))
                     ""
                     (concatenate
                      'string
                      "&nbsp;"
                      (draw-signal (entity-combinator-signal-output entity))
                      "&nbsp;=")))
                (if (entity-combinator-value-p entity)
                    "input"
                    "1"))))

(defmethod entity-graphviz (port (entity entity-combinator-arithmetic) &key &allow-other-keys)
  (format port "~A [label=<<table><tr>~{<td border=\"0\">~A</td>~}</tr></table>>];~%"
          (entity-symbol entity)
          (if (eql (entity-combinator-signal-a entity)
                   (entity-combinator-signal-output entity)) ;Simplify += case
              (if (eql (entity-combinator-signal-output entity)
                       :signal-each) ;Simplify applicable-to-all
                  ;; simplify buffer combinators into nothing
                  (if (or (and (member (entity-combinator-operator entity) (list :+ :-))
                               (equal (entity-combinator-signal-b entity) 0))
                          (and (eql (entity-combinator-operator entity) :*)
                               (equal (entity-combinator-signal-b entity) 1)))
                      (list "")
                      (list 
                        (concatenate 'string (case (entity-combinator-operator entity)
                                               (:- "&minus;")
                                               (t (symbol-name (entity-combinator-operator entity))))
                                     "=")
                        (draw-signal (entity-combinator-signal-b entity))))
                  (list (draw-signal (entity-combinator-signal-output entity))
                        (concatenate 'string (case (entity-combinator-operator entity)
                                               (:- "&minus;")
                                               (t (symbol-name (entity-combinator-operator entity))))
                                     "=")
                        (draw-signal (entity-combinator-signal-b entity))))
              (list (draw-signal (entity-combinator-signal-output entity))
                    "="
                    (draw-signal (entity-combinator-signal-a entity))
                    (entity-combinator-operator entity)
                    (draw-signal (entity-combinator-signal-b entity))))))

(defvar *seen-nets*)

(defmethod entity-graphviz :after (port (entity entity-circuit-input-mixin) &key &allow-other-keys)
  (flet
      ((draw (part color)
         (let* ((circuit (funcall part (entity-circuit-input entity)))
                (inputs (when circuit (circuit-inputs circuit))))
           (when inputs
             (if (or (consp (cdr inputs))
                     (and (circuit-outputs circuit)
                          (consp (cdr (circuit-outputs circuit)))))
                 ;; Complex circuit (multiple inputs or multiple outputs), render as discrete node
                 (progn
                   (when ;; (eql (car inputs) entity)
                       (not (gethash (circuit-symbol circuit) *seen-nets*))
                     (setf (gethash (circuit-symbol circuit) *seen-nets*) t)
                     (format port "~A [label=\"\"];~%" (circuit-symbol circuit))
                     (format port "~{~{~A -> ~A [color=~A dir=both arrowhead=none arrowtail=noneoinv headclip=false];~%~}~}"
                             (mapcar (lambda (x)
                                       (list (entity-symbol x) (circuit-symbol circuit) color))
                                     (remove-if
                                      (lambda (x)
                                        (member x (circuit-inputs circuit)))
                                      (circuit-outputs circuit)))))
                   (if (member entity (circuit-outputs circuit))
                       (format port "~A -> ~A [color=~A headclip=false dir=both arrowtail=dot arrowhead=noneodot];~%#~A -> ~A [color=~A];~%"
                               (entity-symbol entity) (circuit-symbol circuit) color
                               (entity-symbol entity) (entity-symbol entity) color)
                       (format port "~A -> ~A [color=~A tailclip=false];~%"
                               (circuit-symbol circuit) (entity-symbol entity) color)))
                 ;; Simple circuit (A->B), render directly
                 ;; Test for A<->B case first
                 (when (circuit-outputs circuit)
                   (if (let* ((revoutput (first (circuit-outputs circuit)))
                              (revcircuit
                                (and revoutput (funcall part (entity-circuit-input revoutput)))))
                         (and revcircuit
                              (eql (first (circuit-outputs revcircuit)) entity)
                              (not (cdr (circuit-outputs revcircuit)))))
                       (when (string< (symbol-name (entity-symbol entity))
                                      (entity-symbol (first (circuit-outputs circuit))))
                         (format port "~A -> ~A [color=\"~A:~A\" dir=both arrowtail=dot arrowhead=dot];~%"
                                 (entity-symbol (first (circuit-outputs circuit)))
                                 (entity-symbol entity)
                                 color color))
                       (format port "~A -> ~A [color=~A dir=both arrowtail=noneoinv];~%"
                               (entity-symbol (first (circuit-outputs circuit)))
                               (entity-symbol entity)
                               color))))))))
    (draw #'car "red")
    (draw #'cdr "green")))

(defun factorio-graphviz (port &rest entities)
  (let ((seen-ents (make-hash-table))
        (*seen-nets* (make-hash-table)))
    (labels
        ((recurse (ents)
           (dolist (e ents)
             (unless (gethash e seen-ents)
               (setf (gethash e seen-ents) t)
               (with-simple-restart
                   (skip "Skip entity ~A" e)
                 (entity-graphviz port e))
               (dolist (m '((entity-circuit-input-mixin . entity-circuit-input)
                            (entity-circuit-output-mixin . entity-circuit-output)))
                 (when (typep e (car m))
                   (dolist (side (list #'car #'cdr))
                     (let ((c (funcall side (funcall (cdr m) e))))
                       (when c
                         (recurse (circuit-inputs c))
                         (recurse (circuit-outputs c)))))))
               ))))
      (format port "digraph G {~%node [shape=none width=0 height=0 margin=0];~%")
      ;;(recurse entities)
      (dolist (e entities)
        (entity-graphviz port e))
      (format port "}~%"))))
