#lang racket
(define public_key (cons 0 0))
(define make-rsa
  (lambda (p q)
    (if (and (prime? p) (prime? q))
        (set!
              public_key
              (cons (* p q) (make-e p q)))
        (set! public_key (cons 0 0)))))

(define (make-e p q)
  (- (* (- p 1) (- q 1)) 1))

(define (prime? p)
  (define (prime-helper i)
    (if (or (= (remainder p i) 0)
            (= (remainder p (+ i 2)) 0))
            #f
            (if (<= (* i i) p)
                (prime-helper (+ i 6))
                #t)))
  (cond ((> 0 p) #f)
        ((= 1 p) #f)
        ((= 2 p) #t)
        ((= 3 p) #t)
        ((= (remainder p 2) 0) #f)
        ((= (remainder p 3) 0) #f)
        (else (prime-helper 5))))
(define get-rsa
  (lambda (m) (m public_key)))