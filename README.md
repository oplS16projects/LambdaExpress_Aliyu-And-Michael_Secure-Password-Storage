# LambdaPassword

##Authors
Michael Bowe @mgbowe1

Aliyu Zakari @ABZaxxon

##Overview
LambdaPassword is a secure password and username storage that uses the RSA cryptosystems to encrypt the information entered for storage and decrypt the information when requested.

##Screenshot
(insert a screenshot here. You may opt to get rid of the title for it. You need at least one screenshot. Make it actually appear here, don't just add a link.)

Here's a demonstration of how to display an image that's uploaded to this repo:
![screenshot showing env diagram](withdraw.png)

##Concepts Demonstrated
* **Data abstraction**, **recursive data structures** and **encapsulation**  implemented using Message Passing making making the RSA encryption and decryption algorithm which is used as the constructor for the make-db object. We also made helper files that encode and decode the strings.

##External Technology and Libraries
(require math/number-theory) - For the algorithm of the encryption and decryption.
(require racket/gui/base) - For the UI.


##Favorite Scheme Expressions
####Michael Bowe
Each team member should identify a favorite expression or procedure, written by them, and explain what it does. Why is it your favorite? What OPL philosophy does it embody?
Remember code looks something like this:
```scheme
(map (lambda (x) (foldr compose functions)) data)
```
####Aliyu Zakari
This code does the writing of the encryption of the information to a database.ss file. It was fun finally getting it to work and also trying to set such that when information is read in, it is read in as a list with a newline after each entry. It was thanks to Michael for finding a much better methode for the output. It helped eliminate the bug of printing out nothing for no reason.
```scheme
define (input into)
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
You may want to link to your latest release for easy downloading by people (such as Mark).

Include what file to run, what to do with that file, how to interact with the app when its running, etc.
