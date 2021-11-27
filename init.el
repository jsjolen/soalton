;; -*- lexical-binding: t -*-

"
Port of Coalton to Emacs.
This prelude includes all ports for Coalton to load.
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


(defmacro asdf:defsystem (name &rest args)
  "Basic linear loading of a system"
  `(do-load ,(plist-get args :pathname) ',(plist-get args :components)))

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
               (:module "library"
                :serial t
                :components ((:file "macros")
                             (:file "types")
                             (:file "classes")
                             (:file "boolean")
                             (:file "builtin")
                             (:file "fraction")
                             (:file "arith")
                             (:file "char")
                             (:file "string")
                             (:file "optional")
                             (:file "list")
                             (:file "tuple")
                             (:file "result")
                             (:file "functions")
                             (:file "cell")
                             (:file "vector")
                             (:file "graph")))
               (:file "toplevel-environment"))))
