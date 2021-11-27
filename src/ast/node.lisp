(in-package :coalton-impl/ast)

(serapeum:defstruct-read-only
    (node
     (:constructor nil))
  (unparsed :type t))

(defun node-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-p x)))

(deftype node-list ()
  '(satisfies node-list-p))

(defun binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (b) (typep b '(cons symbol node))) x)))

(deftype binding-list ()
  `(satisfies binding-list-p))

(defun symbol-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'symbolp x)))

(deftype symbol-list ()
  '(satisfies symbol-list-p))


;;;;;;;;;;;;;;;;;;;;;;;;; The types of nodes ;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype literal-value ()
  "Allowed literal values as Lisp objects."
  '(or integer single-float double-float string character))


(serapeum:defstruct-read-only
    (node-literal
     (:include node)
     (:constructor node-literal (unparsed value)))
  "A literal value. These include things like integers and strings."
  (value :type literal-value))

(serapeum:defstruct-read-only
    (node-variable
     (:include node)
     (:constructor node-variable (unparsed name)))
  (name :type symbol))


(serapeum:defstruct-read-only
    (node-application
     (:include node)
     (:constructor node-application (unparsed rator rands)))
  (rator :type node)
  (rands :type node-list))

(serapeum:defstruct-read-only
    (node-abstraction
     (:include node)
     (:constructor node-abstraction (unparsed vars subexpr name-map)))
  (vars    :type symbol-list)
  (subexpr :type node)
  (name-map :type list))

(serapeum:defstruct-read-only
    (node-let
     (:include node)
     (:constructor node-let (unparsed bindings subexpr name-map)))
  (bindings :type binding-list)
  (subexpr  :type node)
  (name-map :type list))


(serapeum:defstruct-read-only
    (node-lisp
     (:include node)
     (:constructor node-lisp (unparsed type variables form)))
  (type :type t)
  (variables :type list)
  (form :type t))

(serapeum:defstruct-read-only match-branch
  (unparsed :type t)
  (pattern :type pattern)
  (subexpr :type node)
  (name-map :type list))

(serapeum:defstruct-read-only
    (node-match
     (:include node)
     (:constructor node-match (unparsed expr branches)))
  (expr     :type node)
  (branches :type list))

(serapeum:defstruct-read-only
    (node-seq
     (:include node)
     (:constructor node-seq (unparsed subnodes)))
     (subnodes :type node-list))

(serapeum:defstruct-read-only
    (node-the
     (:include node)
     (:constructor node-the (unparsed type subnode)))
  (type :type t)
  (subnode :type node))
