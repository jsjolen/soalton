;; -*- lexical-binding: t -*-

"
Port of Coalton to Emacs.
This prelude includes all ports for Coalton to load.

Stuff that's difficult:

1. Coalton uses the keyword package to recognise type variables:
;; tyvar := symbol in the keyword package (e.g. :a, :b, etc.)
We have to replace this.
"

;(package-install 'cl-format)

(defmacro TODO (&rest args)
  "Don't know what to do? TODO it!"
  (print (format "TODO: %s" args))
  `(progn))

;;;; PACKAGES
(defmacro in-package (&rest args)
  (progn))
(defmacro defpackage (&rest args)
  (progn))
(defmacro uiop:define-package (&rest args)
  (progn))

;;;; TYPES

(defmacro declaim (&rest args)
  `(cl-declaim ,@args))

(defmacro deftype (&rest args)
  `(cl-deftype ,@args))
(defun walk (f form)
  nil)
(defmacro loop (&rest args)
  `(cl-loop ,@(cl-loop for x in args
                     collect (if (and (symbolp x) (char= (aref (symbol-name x) 0) ?:))
                                 (intern (substring (symbol-name x) 1))
                               x))))

(defun coalton-impl/typechecker::parse-type-definitions (&rest args)
  (apply 'parse-type-definitions args))
(defun coalton-impl/typechecker::derive-bindings-type (&rest args)
  (apply 'derive-bindings-type args))
(defun coalton-impl/typechecker::apply-substitution (&rest args)
  (apply 'apply-substitution args))


(defmacro coalton-impl/typechecker::with-type-context (&rest args)
  `(with-type-context ,@args))
(defun make-array (len)
  (make-vector len nil))



;;;; Conditions

(defmacro define-condition (name superclass &rest args)
  `(define-error ',name ,(format "COALTON ERROR %s" name) ',(first superclass)))

;;;; Utils
(defun first (list) (car list))
(defun second (list) (car (cdr list)))
(defun third (list) (car (cddr list)))
(defun fourth (list) (car (cdddr list)))
(defun fifth (list) (car (cddddr list)))
(defun sixth (list) (car (cdr (cddddr list))))
(defun seventh (list) (car (cddr (cddddr list))))
(defun eighth (list) (car (cdddr (cddddr list))))
(defun ninth (list) (car (cddddr (cddddr list))))
(defun tenth (list) (car (cdr (cddddr (cddddr list)))))
(defun endp (list)
  (null list))
(defun rest (list)
  (cdr list))

(defun uiop:getenv (env-var)
  (getenv env-var))

(defmacro check-type (&rest args)
  (progn))
(defmacro etypecase (&rest args)
  `(cl-etypecase ,@args))
(defmacro defstruct (&rest args)
  `(cl-defstruct ,@args))

(defmacro serapeum:defstruct-read-only (&rest args)
  "This definition works in immutable-map.lisp"
  `(cl-defstruct ,@args))
;;;;; alexandria
(defmacro alexandria:define-constant (&rest args)
  `(defconst ,(car args) ,(cadr args)))
(defun alexandria:make-gensym (x)
  (gensym x))
(cl-defun alexandria:make-gensym-list (len &optional (g "G"))
  (cl-loop for i from 0 upto (1- len) collect (alexandria:make-gensym g)))
(defun alexandria:hash-table-keys (ht)
  (let (ks)
    (maphash (lambda (k v) (push k ks)) ht)
    ks))

;; Stole this from Alexandria, thx!
(defun alexandria:proper-list-p (object)
  "Returns true if OBJECT is a proper list."
  (cond ((not object)
         t)
        ((consp object)
         (do ((fast object (cddr fast))
              (slow (cons (car object) (cdr object)) (cdr slow)))
             (nil)
           (unless (and (listp fast) (consp (cdr fast)))
             (return (and (listp fast) (not (cdr fast)))))
           (when (eq fast slow)
             (return nil))))
        (t
         nil)))

(defun alexandria:ensure-symbol (n p)
  (print p) ;; What packages is this called with???
  (gensym (symbol-name n)))

(defmacro do (&rest args)
  `(cl-do ,@args))
(defmacro return (&rest args)
  `(cl-return ,@args))
(defun find-package (&rest a)
  nil)
(defun symbol-package (&rest a)
  nil)
(defun copy-list (&rest args)
  (apply 'cl-copy-list args))
(defun intersection (&rest args)
  (apply 'cl-intersection args))
(defun find (&rest args)
  (apply 'cl-find args))
(defmacro incf (&rest args)
  `(cl-incf ,@args))
(defun every (&rest args)
  (apply 'cl-every args))
(defun concatenate (&rest args)
  (apply 'cl-concatenate args))
(defun union (&rest args)
  (apply 'cl-union args))

;;;; FSet
(cl-defstruct soalton-map
  ht)
(cl-defstruct soalton-set
  ht)
;; TODO: What is default? FSet docs
(cl-defun fset:empty-map (&optional default)
  (make-soalton-map :ht (make-hash-table)))
(cl-defmacro fset:map (&rest init)
  `(let* ((map (fset:empty-map))
          (m (soalton-map-ht map)))
    ,@(cl-loop for (k v) in init collect
      `(setf (gethash ,k m) ,v))
    map))
(cl-defun fset:empty-seq (&rest args)
  nil)
(cl-defgeneric fset:with (c v1 &optional v2))
(cl-defmethod fset:with ((c soalton-map) k &optional v)
  (let ((ht-new (copy-hash-table (soalton-map-ht c))))
    (setf (gethash k ht-new) v)
    (make-soalton-map :ht ht-new)))
(cl-defgeneric fset:lookup ((c v)))
(cl-defmethod fset:lookup ((c soalton-map) v)
  (gethash v (soalton-map-ht c)))

(cl-defun char= (a b)
  (char-equal a b)) 
(cl-defun char (str n)
  (aref str n))

;(defmacro cl:defmacro (&rest args)
;  `(cl-defmacro ,@args))

;(defmacro cl:labels (&rest args)
;  `(cl-labels ,@args))

;(defmacro cl:if (&rest args)
;  `(if ,@args))

;(defun cl:null (x) (null x))

;(defun cl:cdr (x) (cdr x))

(defun format-symbol (str &rest args)
  (intern (apply 'format str args)))

;; TODO: Diff between the two?
(defmacro define-global-var (name value)
  `(defvar ,name ,value))
(defmacro defparameter (name value)
  `(defvar ,name ,value))

(defmacro asdf:defsystem (name &rest args)
  "Basic linear loading of a system"
  `(do-load ,(plist-get args :pathname) ',(plist-get args :components)))

(defmacro assert (&rest args)
  `(cl-assert ,@args))

(defmacro values (&rest args)
  `(cl-values ,@args))

(defun do-load (pathname components)
  (mapcar
   (lambda (cmpt)
     (cond
      ((eq (car cmpt) :file)
       (print (cadr cmpt))
       (load (concat default-directory pathname (cadr cmpt) ".lisp")))
      ((eq (car cmpt) :module)
       (print (concat pathname (cadr cmpt) "/"))
       (do-load (concat pathname (cadr cmpt) "/")
                (plist-get cmpt :components)))))
   components))

;;;; Load Coalton
(setf sys
'(asdf:defsystem :coalton
  :description "An efficient, statically typed functional programming language that supercharges Common Lisp. "
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :depends-on (:alexandria
               :global-vars
               :trivia
               :serapeum
               :fset
               :float-features)
  :in-order-to ((asdf:test-op (asdf:test-op :coalton/tests)))
  :around-compile (lambda (compile)
                    (let (+sbcl (sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "global-lexical")
               (:file "settings")
               (:module "algorithm"
                :serial t
                :components ((:file "tarjan-scc")
                             (:file "immutable-map")
                             (:file "immutable-listmap")))
               (:module "ast"
                :serial t
                :components ((:file "pattern")
                             (:file "node")
                             (:file "parse-error")
                             (:file "parse-form")
                             (:file "free-variables")))
               (:module "typechecker"
                :serial t
                :components ((:file "kinds")
                             (:file "types")
                             (:file "pretty-printing")
                             (:file "substitutions")
                             (:file "predicate")
                             (:file "scheme")
                             (:file "typed-node")
                             (:file "type-errors")
                             (:file "unify")
                             (:file "equality")
                             (:file "environment")
                             (:file "context-reduction")
                             (:file "type-parse-error")
                             (:file "parse-type")
                             (:file "parse-type-definition")
                             (:file "parse-class-definition")
                             (:file "parse-instance-definition")
                             (:file "derive-type")
                             (:file "check-types")
                             (:file "debug")))
               (:module "codegen"
                :serial t
                :components ((:file "utilities")
                             (:file "lisp-types")
                             (:file "function-entry")
                             (:file "optimizer")
                             (:file "direct-application")
                             (:file "match-constructor-lift")
                             (:file "pointfree-transform")
                             (:file "compile-pattern")
                             (:file "compile-typeclass-dicts")
                             (:file "compile-expression")
                             (:file "compile-type-definition")
                             (:file "program")))
               (:file "toplevel-define-type")
               (:file "toplevel-declare")
               (:file "toplevel-define")
               (:file "toplevel-define-instance")
               (:file "coalton")
               (:file "debug")
               (:module "doc"
                :serial t
                :components ((:file "generate-documentation")))
               (:file "faux-macros")
               ;; (:module "library"
               ;;  :serial t
               ;;  :components ((:file "macros")
               ;;               (:file "types")
               ;;               (:file "classes")
               ;;               (:file "boolean")
               ;;               (:file "builtin")
               ;;               (:file "fraction")
               ;;               (:file "arith")
               ;;               (:file "char")
               ;;               (:file "string")
               ;;               (:file "optional")
               ;;               (:file "list")
               ;;               (:file "tuple")
               ;;               (:file "result")
               ;;               (:file "functions")
               ;;               (:file "cell")
               ;;               (:file "vector")
               ;;               (:file "graph")))
                (:file "toplevel-environment")
               )))
