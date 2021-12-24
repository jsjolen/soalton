(in-package #:coalton-impl/typechecker)

;;;
;;; Type equality
;;;

(defun type= (type1 type2)
  (declare (type ty type1 type2)
           (values boolean list))
  (let ((var-table nil))
    (cl-labels ((%type= (type1 type2)
      (cond
        ;; Type variables
        ((and (tvar-p type1)  (tvar-p type2))
         (let* ((pair1 (find type1 var-table :key #'car :test #'equalp))
                (pair2 (find type2 var-table :key #'car :test #'equalp)))
           (cond
             ((and (null pair1) (null pair2))
                 (pushnew (cons type1 type2) var-table :key #'car :test #'equalp)
                 (pushnew (cons type2 type1) var-table :key #'car :test #'equalp)
                 t)
             ((or (null pair1) (null pair2))
              nil)
             (t
              (and (eql (car pair1) (cdr pair2))
                   (eql (cdr pair1) (car pair2)))))))


        ;; Type constants
        ((and (tcon-p type1) (tcon-p type2)) (equalp type1 type2))

        ;; Type application
        ((and (tapp-p type1) (tapp-p type2))
         (and (%type= (tapp-from type1) (tapp-from type2))
              (%type= (tapp-to type1) (tapp-to type2))))

        (t nil))))
      (let ((ret (%type= type1 type2)))
        (values ret var-table)))))

(defun qualified-type= (qual-type1 qual-type2)
  ;; Check that the types are equal
  (cl-multiple-value-bind (types-equal-p var-table)
      (type= (qualified-ty-type qual-type1)
             (qualified-ty-type qual-type2))
    (unless types-equal-p
      (return-from qualified-type= nil))

    ;; Create a substitution list from the variables
    (let ((subs-list (mapcar (lambda (s)
                               (%make-substitution (tvar-tyvar (car s)) (cdr s)))
                             var-table)))

      ;; Now check that all constraints in type1 exist in type2, mapping type variables
      (loop :for pred :in (qualified-ty-predicates qual-type1) :do
        (unless (cl-member (apply-substitution subs-list pred)
                        (qualified-ty-predicates qual-type2)
                        :test #'equalp)
          (return-from qualified-type= nil)))
      ;; And do the same for constraints in type2
      (loop :for pred :in (qualified-ty-predicates qual-type2) :do
        (unless (cl-member (apply-substitution subs-list pred)
                        (qualified-ty-predicates qual-type1)
                        :test #'equalp)
          (return-from qualified-type= nil)))))

  t)

(defun type-scheme= (scheme1 scheme2)
  (qualified-type=
   (fresh-inst scheme1)
   (fresh-inst scheme2)))

(defmethod fset:compare ((x ty-scheme) (y ty-scheme))
  (if (type-scheme= x y)
      :equal
      :unequal))

(defun type-predicate= (pred1 pred2)
  (and (eql (ty-predicate-class pred1)
            (ty-predicate-class pred2))
       (every (lambda (x y)
                (type= x y))
              (ty-predicate-types pred1)
              (ty-predicate-types pred2))))
