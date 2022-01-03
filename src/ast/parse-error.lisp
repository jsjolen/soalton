(in-package #:coalton-impl/ast)

(define-condition coalton-parse-error (error)
  ((form :initarg :form
         :reader coalton-parse-error-form)
   (reason-control :initarg :reason-control
                   :reader coalton-parse-error-reason-control)
   (reason-args :initarg :reason-args
                :reader coalton-parse-error-reason-args))
  (:report (lambda (c s)
             (let ((*print-pretty* nil))
               (format s "Failed to parse ~S~%    ~?"
                       (coalton-parse-error-form c)
                       (coalton-parse-error-reason-control c)
                       (coalton-parse-error-reason-args c))))))

(define-condition coalton-parse-error-context (coalton-parse-error)
  ((context :initarg :context
            :reader coalton-parse-error-context
            :type string)
   (suberror :initarg :suberror
             :reader coalton-parse-error-suberror
             :type coalton-parse-error))
  (:documentation "A coalton parse error with additional context")
  (:report
   (lambda (c s)
     (format s "~A~%in ~A"
             (coalton-parse-error-suberror c)
             (coalton-parse-error-context c)))))

(cl-defmacro with-parsing-context ((context &rest args) &rest body)
  `(condition-case c
       (progn ,@body)
     (coalton-parse-error
      (signal 'coalton-parse-error-context
              (list
               :context (format ,context ,@args)
               :suberror c)))
     (coalton-impl/typechecker::coalton-type-error
       (signal 'coalton-parse-error-context
               (list
                :context (format ,context ,@args)
                :suberror) c))))

(defun error-parsing (form reason-control &rest reason-args)
  (signal 'coalton-parse-error
   (list
         :form form
         :reason-control reason-control
         :reason-args reason-args)))
