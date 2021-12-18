;; -*- lexical-binding: t -*-

"
Port of Coalton to Emacs.
This prelude includes all ports for Coalton to load.

Stuff that's difficult:

1. Coalton uses the keyword package to recognise type variables:
;; tyvar := symbol in the keyword package (e.g. :a, :b, etc.)
We have to replace this.
"

(package-install 'cl-format)

(defmacro TODO (&rest args)
  "Don't know what to do? TODO it!"
  (print (cl-format nil "TODO: ~a" args))
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


;;;; Conditions

(defmacro define-condition (name superclass &rest args)
  `(define-error ',name ,(format "COALTON ERROR %s" name) ',(first superclass)))

;;;; Utils
(defun first (cons)
  (car cons))

(defun uiop:getenv (env-var)
  (getenv env-var))

(defmacro check-type (&rest args)
  (progn))

(defmacro defstruct (&rest args)
  `(cl-defstruct ,@args))
(defmacro serapeum:defstruct-read-only (&rest args)
  "This definition works in immutable-map.lisp"
  `(cl-defstruct ,@args))

(defmacro alexandria:define-constant (&rest args)
  `(defconst ,(car args) ,(cadr args)))
(defun alexandria:make-gensym (x)
  (gensym x))
(cl-defun alexandria:make-gensym-list (len &optional (g "G"))
  (cl-loop for i from 0 upto (1- len) collect (alexandria:make-gensym g)))

(defun find-package (&rest a)
  nil) 
(defun symbol-package (&rest a)
  nil)

;; TODO: What is default? FSet docs
(cl-defun fset:empty-map (&optional default)
  (make-hash-table))
(cl-defmacro fset:map (&rest init)
  `(let ((m (fset:empty-map)))
    ,@(cl-loop for (k v) in init collect
      `(setf (gethash ,k m) ,v))
    m))
(cl-defun fset:empty-seq (&rest args)
  nil)

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

(defun do-load (pathname components)
  (mapcar
   (lambda (cmpt)
     (cond
      ((eq (car cmpt) :file)
       (print (cadr cmpt))
       (byte-compile-file (concat default-directory pathname (cadr cmpt) ".lisp") t))
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
               ;; (:file "toplevel-environment")
               )))
