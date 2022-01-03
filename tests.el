(coalton:coalton-toplevel (coalton:define (id x) x))
(coalton:coalton-toplevel
        (coalton:define (id2 x)
          (coalton:lisp coalton:Unit () (print "hi!"))))
(coalton:coalton-toplevel (coalton:define (square x) (* x x)))
