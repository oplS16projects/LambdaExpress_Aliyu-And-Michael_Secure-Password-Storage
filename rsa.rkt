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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-database info)
  (define myrsa (make-rsa prime1 prime2 'iloveopl))
  
  (define (search website)  ;; procedure not fuctioning, Database object has to be made
    (map (lambda(x) (decode ((myrsa 'decrypt '(78951163299089459819508873) 'iloveopl))))
         (filter (equal? (lambda (x) (myrsa 'encrypt '(112102626))info)
                         ))))
  
  (define (store website username password)  ;; procedure not fuctioning, Database object has to be made
    (cons (myrsa 'encrypt (encode website)) (list (myrsa 'encrypt (encode username)) (myrsa 'encrypt (encode password)) )))
  
  (define (retrieve x)  ;; procedure not fuctioning, Database object has to be made
    (map (lambda(x) (decode (decode x)))
         (filter (equal? (lambda (x) (decode(car search))info)
                         ))))
  
  (define (dispatch m . params)
    (cond ((equal? m 'search) (search params))
          ((equal? m 'store) (store params params params))
          ((equal? m 'retrieve) (store  params))
          ))
  
  dispatch)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;<<<<<<< HEAD
(define prime1 17541956566777) ;; prime numbers
(define prime2 9971149563847)  ;; prime numbers

;   (define (search x)  ;; procedure not fuctioning, Database object has to be made
;     (map (lambda(x) (decode (decrypt x)))
;          (filter (equal? (lambda (x) (encrypt(car search))database)
;                          ))))

;; Instructions on how to use the following procedures.

;;(define myrsa (make-rsa 1st_large_prime_number 2nd_large_prime_number password_symbol)) ;; makes an rsa object
;;Example (define myrsa (make-rsa prime1 prime2 'iloveopl))

;;(myrsa 'encrypt <some_list_of_numbers>) ;; encrypts numbers
;;Example (myrsa 'encrypt '(112102626))

;;(myrsa 'decrypt <some_list_of_numbers> 'foo) ;; decrypts numbers
;;Example (myrsa 'decrypt '(78951163299089459819508873) 'iloveopl)

;;(myrsa 'passwd 'old_password_symbol 'new_password_symbol) ;; changes the password
;;Example (myrsa 'passwd 'iloveopl 'ilovecats)

;;(myrsa 'key) ;; returns the public key
;;Example (myrsa 'key)

;;(encode string_to_be_encoded)
;;Example (encode "www.amazon.com")

;;(decode number_generated_from_encoding_string)
;;Example (decode 109111099046110111122097109097046119119119)
;=======
;; database
(define db
  (lambda (p q pass)
    (define data '())
    (define internal-encryption (make-rsa p q pass))
    (define insert
      (lambda (list-data)
        (if (string? (caar list-data))
            (input(cons (internal-encryption 'encrypt (map encode (car list-data))) data))
            (input(cons (internal-encryption 'encrypt (map encode (car list-data))) data))
            ;(set! data (cons (internal-encryption 'encrypt (map encode (car list-data))) data))
            ;(set! data (cons (internal-encryption 'encrypt (car list-data)) data))
            )))
    (define retrieve
      (lambda (retrival passwd)
        (if (equal? pass passwd)
            (if (string? retrival)
                (map (lambda (x) (map decode x)) (map (lambda (x) (internal-encryption 'decrypt x pass))
                                                      (filter (lambda (x)
                                                                (equal?
                                                                 (internal-encryption 'encrypt (list (encode retrival)))
                                                                 (list (car x))))
                                                              data)))
                (map (lambda (x) (internal-encryption 'decrypt x pass))
                     (filter (lambda (x)
                               (equal?
                                (internal-encryption 'encrypt (list (encode retrival)))
                                (list (car x))))
                             data)))
            "ERROR: wrong password")))

    
    (define (dispatch m . args)
      (cond ((eq? m 'insert) (insert args))
            ((eq? m 'retrieve) (retrieve (car args) (cadr args)))
            (else "ERROR: invalid operation")))
    (if (and (prime? p) (prime? q))
        dispatch
        "ERROR: p and q must be prime")))

(define (input into) 
    (let ((p (open-output-file "database.ss" #:exists 'append)))
  (let f ((ls (list into)))    
    (if (not (null? ls))
        (begin
          (write (car(car ls)) p)
          (newline p)
          (f (cdr ls))) null))
  (close-output-port p))
  )

(define output
  (call-with-input-file "database.ss" 
  (lambda (p)
    (let f ((x (read p)))
      (if (list? x)
          (cons x (f (read p)))
          '()))))
  )

;>>>>>>> master
(define password (db prime1 prime2 'abz))
(password 'insert (list "amazon.com" "leo" "1234"))
(password 'insert (list "amazon.com" "meo" "1234"))
(password 'insert (list "amazon.com" "keo" "1234"))
(password 'insert (list "walmart.com" "aeo" "1234"))
(password 'insert (list "walmart.com" "seo" "1234"))
(password 'insert (list "walmart.com" "deo" "1234"))
(password 'retrieve  "amazon.com" 'abz)
(password 'retrieve  "walmart.com" 'abz)

;(input (list 1 2 3 4 5))