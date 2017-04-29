(in-package :factorio-circuits.entity)

;; Inputs is a list of entity inputs, NOT a list of inputs-to-the-network (i.e. entity outputs)
(defclass circuit-net ()
  ((circuit-inputs :accessor circuit-inputs :type cons :initform (list))
   (circuit-outputs :accessor circuit-outputs :type cons :initform (list))
   (circuit-symbol :accessor circuit-symbol :type symbol :initform (gensym "NET"))))

(defun circuit-net-merge (this other color)
  (dolist (o-i (circuit-inputs other))
    (setf (circuit-inputs this)
          (cons o-i (circuit-inputs this)))
    (if (eql color :red)
        (setf (car (entity-circuit-input o-i)) this)
        (setf (cdr (entity-circuit-input o-i)) this)))
  (dolist (o-o (circuit-outputs other))
    (setf (circuit-outputs this)
          (cons o-o (circuit-outputs this)))
    (if (eql color :red)
        (setf (car (entity-circuit-output o-o)) this)
        (setf (cdr (entity-circuit-output o-o)) this))))

(defun entity-connect (a b color)
  (declare (entity-circuit-output-mixin a)
           (entity-circuit-input-mixin b)
           ((member :red :green) color))
  (flet ((side (x) (if (eql color :red) (car x) (cdr x)))
         ((setf side) (y x) (if (eql color :red) (setf (car x) y) (setf (cdr x) y))))
    (let* ((a-o (entity-circuit-output a))
           (b-i (entity-circuit-input b)))
      (cond ((not (or (side a-o) (side b-i))) ;No networks
             (setf (side a-o) (make-instance 'circuit-net))
             (setf (side b-i) (side a-o)
                   (circuit-outputs (side a-o)) (list a)
                   (circuit-inputs (side b-i)) (list b)))

            ((eq (side a-o) (side b-i))     ;Same network
             (warn "Entity connection ~A -> ~A already on the same ~(~A~) net (~A)"
                   a b color (side a-o)))
            
            ((and (side a-o) (not (side b-i))) ;A has a network, B doesn't
             (setf (circuit-inputs (side a-o))
                   (cons b (circuit-inputs (side a-o))))
             (setf (side b-i) (side a-o)))

            ((and (side b-i) (not (side a-o))) ;B has a network, A doesn't
             (setf (circuit-outputs (side b-i))
                   (cons a (circuit-outputs (side b-i))))
             (setf (side a-o) (side b-i)))

            (t ;Worst-case: both have different networks -- we have to merge them.
             (circuit-net-merge (side a-o) (side b-i) color))))))

(defmacro entity-connect-type (type)
  `(defun ,(intern (format nil "~:@(entity-connect-~As~)" type)) (a b color)
     (declare (,(intern (format nil "~:@(entity-circuit-~A-mixin~)" type)) a b)
              ((member :red :green) color))
     (flet ((side (x) (if (eql color :red) (car x) (cdr x)))
            ((setf side) (y x) (if (eql color :red) (setf (car x) y) (setf (cdr x) y))))
       (let* ((a-t (,(intern (format nil "~:@(entity-circuit-~A~)" type)) a))
              (b-t (,(intern (format nil "~:@(entity-circuit-~A~)" type)) b)))
         (cond ((not (or (side a-t) (side b-t))) ;No networks
                (setf (side a-t) (make-instance 'circuit-net))
                (setf (side b-t) (side a-t)
                      (,(intern (format nil "~:@(circuit-~As~)" type)) (side a-t)) (list a b)))

               ((eq (side a-t) (side b-t))     ;Same network
                (warn "Entity connection ~A:I -> ~A:I already on the same ~(~A~) net (~A)"
                      a b color (side a-t)))
               
               ((and (side a-t) (not (side b-t))) ;A has a network, B doesn't
                (setf (,(intern (format nil "~:@(circuit-~As~)" type)) (side a-t))
                      (cons b (,(intern (format nil "~:@(circuit-~As~)" type)) (side a-t))))
                (setf (side b-t) (side a-t)))

               ((and (side b-t) (not (side a-t))) ;B has a network, A doesn't
                (setf (,(intern (format nil "~:@(circuit-~As~)" type)) (side b-t))
                      (cons a (,(intern (format nil "~:@(circuit-~As~)" type)) (side b-t))))
                (setf (side a-t) (side b-t)))

               (t ;Worst-case: both have different networks -- we have to merge them.
                (circuit-net-merge (side a-t) (side b-t) color)))))))

(entity-connect-type "input")
(entity-connect-type "output")
