#lang racket
;This program gives random numbers for people based on the choice of what was asked.
(define programExec "none")
(define listP "none")
(define places "none")
;we start the program
(define categories '((Select 1 for Choose_number_Of_Places_InExamen_Or_Stade)  (Select 2 for ChooseTeam_Of_Npersons) (Select 3 for Leave_the_program)));here it what the program can do
(display "Please choose among our program's function")
(newline)
(display "This what our function can do :")
(display categories)
(newline)

(define (MakeRightChoose);this function is for get the choose for the user 
  (display  "Enter your selection number (means between 1 2 or 3)")
  (newline)
  (define choose (string->number(read-line)))
  (cond
   ((or (equal? 1 choose) (equal? 2 choose) (equal? 3 choose));when you enter a good choose 
   (set! programExec choose)
   )
   (else
    (display "You must insert a number between 1 2 or 3,Please try again")
    (newline)
   (MakeRightChoose)
   )
  ))
(MakeRightChoose)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (save 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (delet element list res);this function delet element in a list
  (cond
    ((null? list) (reverse res '()))
    ((equal? element (car list))
     (delet element (cdr list) res))
    (else
     (delet element (cdr list) (cons (car list) res))
   )))
(define (reverse list new)
  (if
   (null? list) new
   (reverse (cdr list) (cons (car list) new))))
(define (getElementAt index list start);this fonction return an element in a specifique places 
  (if
   (equal? index start)(car list)
   (getElementAt index (cdr list) (+ start 1))))
(define (length list sum);this function is for length
  (if
   (null? list)sum
   (length (cdr list) (+ sum 1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (makePlaces listPers listPlaces result)
  (cond
    ((null? listPers)
     (display "Those are your results:")
     (newline)
     (display result))
    (else
     (define placeN (getElementAt (random 0 (length listPlaces 0)) listPlaces 0));here we stock the number of place selected
     (makePlaces (delet (car listPers) listPers '()) (delet placeN listPlaces '()) (cons (cons (car listPers) placeN) result))
   )
    ))

(define (choosePlaces);this function give ranom choose
  (newline)
  (display "Please follow all instructions below given with the folder")
  (newline)
  (display "Please,insert name of persons concerned separated by comma each:")
  (newline)
  (define persons (read-line))
  (set! listP (string-split persons ","))
  (display "Please insert places's number(names) equal or superior to number of persons inserted separated by comma each :")
  (newline)
  (define place (read-line))
  (set! places (string-split place ","))
  (cond
   ((> (length listP 0) (length places 0))
    (display "Please insert more places than persons,for more information check how to do it in the file given : README.txt")
    (choosePlaces))
   (else
     (makePlaces listP places '())
    )
   )
 
  )

(define (chooseTeam);this function
  (display "NOT YET CODED")
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (directionProgram choose);this function sent toward the choose made
  (cond
    ((equal? 1 choose)
      (display "-------------------------------------------------PLACES----------------------------------------------------")
      (choosePlaces)
      (newline)
      (display "--------------------------------------------------DONE-----------------------------------------------------")
     )
    ((equal? 2 choose)
     (chooseTeam)
     )
    (else
     (display "Good-bye")
     )
    )
  )
(directionProgram programExec)

  
  



                     