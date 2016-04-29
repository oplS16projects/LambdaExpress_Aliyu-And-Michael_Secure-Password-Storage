# LambdaPassword

##Authors
Michael Bowe @mgbowe1

Aliyu Zakari @ABZaxxon

##Overview
LambdaPassword is a secure password and username storage that uses the RSA cryptosystem to encrypt the information entered for storage and decrypt the information when requested.

##Screenshot
(insert a screenshot here. You may opt to get rid of the title for it. You need at least one screenshot. Make it actually appear here, don't just add a link.)

Here's a demonstration of how to display an image that's uploaded to this repo:
![screenshot showing env diagram](withdraw.png)

##Concepts Demonstrated
* **Data abstraction**, **recursive data structures** and **encapsulation**  implemented using Message Passing making making the RSA encryption and decryption algorithm which is used as the constructor for the make-db object. We also made helper functions that encode and decode strings.

##External Technology and Libraries
(require math/number-theory) - For the algorithm of the encryption and decryption.


##Favorite Scheme Expressions
####Michael Bowe
This expression is the heart and soul of RSA encryption. It amazes me how elegant this can be. The function definition below takes a list of numbers and encrypts them so they can be easily decrypted with the proper private key.
```scheme
(define (encrypt num)
  (map (lambda (x)
         (modular-expt x (cdr public_key) (car public_key)))
       (car num)))
```
####Aliyu Zakari
This code does the writing of the encryption of the information to a database.ss file. It was fun finally getting it to work and also trying to set such that when information is read in, it is read in as a list with a newline after each entry. It was thanks to Michael for finding a much better methode for the output. It helped eliminate the bug of printing out nothing for no reason.
```scheme
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
(define output (lambda () (file->list "database.ss")))
```


#How to Download and Run
* unzip the [repository](https://github.com/oplS16projects/LambdaExpress_Aliyu-And-Michael_Secure-Password-Storage/releases/tag/v0.2) and create a file named database.ss
* run rsa.rkt in drracket
* use ```(db 'insert '("str1" "str2" "str3" ... "strN"))``` to store your data
* use ```(db 'retrieve "str1" 'foo)``` to get your data where "str1" matches "str1" in your data
* use ```(db 'get-all 'foo)``` to show everything in your database.ss file in a readable format
* use ```(db 'showdata)``` to show everything in your database.ss file
