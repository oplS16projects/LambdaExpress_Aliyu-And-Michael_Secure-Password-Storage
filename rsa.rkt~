#lang racket
(require math/number-theory)
(define (make-rsa p q pass)
  (define (make-e p q)
    (- (* (- p 1) (- q 1)) 1))
  (define public_key (cons (* p q) (make-e p q)))
  (define (encrypt num)
    (map (lambda (x)
           (modular-expt x (cdr public_key) (car public_key)))
         (car num)))
  (define (decrypt message password)
    (if (equal? (car password) pass)
        (map (lambda (x)
               (modular-expt x
                             (modular-inverse (cdr public_key)
                                              (* (- p 1) (- q 1)))
                             (car public_key)))
             message)
        "ERROR: Unable to Authenticate"))
  (define (setpass passwd newpass)
    (if (equal? passwd pass)
        (begin
          (set! pass (car newpass))
          pass)
        "ERROR: Unable to Authenticate"))
  (define (dispatch m . params)
    (cond ((equal? m 'encrypt) (encrypt params))
          ((equal? m 'decrypt) (decrypt (car params) (cdr params)))
          ((equal? m 'passwd) (setpass (car params) (cdr params)))
          ((equal? m 'key) public_key)))
  (if (and (prime? p) (prime? q))
      dispatch
      "ERROR: p and q must be prime"))

;; helpers for string encryption
(define encode
  (lambda (s)
    (string->number (foldl (lambda (a b)
                             (string-append (~a (char->integer a) #:min-width 3 #:align 'right #:left-pad-string "0") b))
                           ""
                           (string->list s)))))
(define (decode n)
  (define (iter n result)
    (let ((c (remainder n 1000)))
      (if (= n 0)
          result
          (iter (quotient n 1000) (string-append
                                   result
                                   (list->string (list (integer->char c)))
                                   )))))
  (iter n ""))