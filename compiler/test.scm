
(define (step state world)
  (cons state 0))

(define (main world ghost-programs)
  (dbug world)
  (cons 0 step))
