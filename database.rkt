#lang racket
(require racket/include)
(include "rsa.rkt")

;; database
(define make-db
  (lambda (p q pass)
    (define data '())
    (define internal-encryption (make-rsa p q pass))
    (define (input into) 
      (let ((p (open-output-file "database.ss" #:exists 'append)))
        (let f ((ls (list into)))    
          (if (not(null? ls))
              (begin
                (write (car(car ls)) p)
                (newline p)
                (f (cdr ls)))null))
        (close-output-port p) )
      )
    (define output (file->list "database.ss"))
    (define insert
      (lambda (list-data)
        (if (string? (caar list-data))
            ;(set! data (cons (internal-encryption 'encrypt (map encode (car list-data))) data))
            ;(set! data (cons (internal-encryption 'encrypt (car list-data)) data)))))
            (input (cons (internal-encryption 'encrypt (map encode (car list-data))) data))
            (input (cons (internal-encryption 'encrypt (car list-data)) data)))))
    (define retrieve
      (lambda (retrival passwd)
        (if (equal? pass passwd)
            (if (string? retrival)
;                (map (lambda (x) (map decode x)) (map (lambda (x) (internal-encryption 'decrypt x pass))
;                                                      (filter (lambda (x)
;                                                                (equal?
;                                                                 (internal-encryption 'encrypt (list (encode retrival)))
;                                                                 (list (car x))))
;                                                              data)))
;                (map (lambda (x) (internal-encryption 'decrypt x pass))
;                     (filter (lambda (x)
;                               (equal?
;                                (internal-encryption 'encrypt (list (encode retrival)))
;                                (list (car x))))
;                             data))
                (map (lambda (x) (map decode x)) (map (lambda (x) (internal-encryption 'decrypt x pass))
                                                      (filter (lambda (x)
                                                                (equal?
                                                                 (internal-encryption 'encrypt (list (encode retrival)))
                                                                 (list (car x))))
                                                              output)))
                (map (lambda (x) (internal-encryption 'decrypt x pass))
                     (filter (lambda (x)
                               (equal?
                                (internal-encryption 'encrypt (list (encode retrival)))
                                (list (car x))))
                             output))
                )
            "ERROR: wrong password")))
    (define get-all
      (lambda (passwd)
        (if (equal? pass passwd)
            (map (lambda (x) (map decode x))
;                 (map (lambda (x) (internal-encryption 'decrypt x pass))
;                      data)
                 (map (lambda (x) (internal-encryption 'decrypt x pass))
                      output))
            "ERROR: wrong password")))
    (define (dispatch m . args)
      (cond ((eq? m 'insert) (insert args))
            ((eq? m 'retrieve) (retrieve (car args) (cadr args)))
;            ((eq? m 'showdata) data)
            ((eq? m 'showdata) output)
            ((eq? m 'get-all) (get-all (car args)))
            (else "ERROR: invalid operation")))
    (if (and (prime? p) (prime? q))
        dispatch
        "ERROR: p and q must be prime")))



;; instantiate a database
(define db (make-db (+ (expt 10 999) 7) (+ (expt 10 999) 663) 'foo))

;;How To run the program
;;(db 'insert (list "www.amazon.com" "leo" "1234"))
;;(db 'retrieve "www.amazon.com" 'foo)