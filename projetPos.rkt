#lang scheme
;This program gives random numbers for people based on the choice of what was asked.
(define programExec "none")
(define listP "none")
(define places "none")
(define team "none")
 (define splitN "none")
(define  response '())
(define border "-------------------------")
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
(define (quesSave)
  (display "Would you like to save the result? Yes or No")
  (newline)
  (define rep (string-upcase(read-line)))
  (cond
    ((equal? rep "YES")
     (save)
      (display "SAVE-SUCCESSFULLY")
     )
    (else
     (display "THANK-YOU"))))

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
;ssve
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (makePlaces listPers listPlaces result)
  (cond
    ((null? listPers)
     (display "Those are places specfics for each persons given:")
     (newline)
     (set! response (reverse result '()))
     (display response))
    (else
     (define placeN (getElementAt (random 0 (length listPlaces 0)) listPlaces 0));here we stock the number of place selected
     (makePlaces (delet (car listPers) listPers '()) (delet placeN listPlaces '()) (cons (cons (car listPers) placeN) result))
   )
    ))
(define (makeTeam numberT listP T start)
  (cond
    ((or(equal? start numberT)(null? listP))
     (cond
      ((null? listP)
       (newline)
       (display T)
       (set! response(cons T response))
       (newline)
       (display "We got ")
       (display (length response 0))
       (display " teams")
       )
      (else
       (newline)
       (display T)
       (set! response(cons T response))
       (makeTeam numberT listP '() 0))))
                     
    
    (else
     (define teamM (getElementAt (random 0 (length listP 0)) listP 0))
     (makeTeam numberT  (delet teamM listP '()) (cons teamM T) (+ start 1)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define(save)
  (display "Insert the location")
  (newline)
  (display "you can see an example in README.txt")
  (newline)
  (define location (read-line))
  (insertIn location response 0)
 )
(define (insertIn loc list start)
  (cond
   ((null? list)
  (call-with-output-file loc
    #:exists 'append
    (lambda (output-port)
      (display border output-port))))
   ((equal? start 0)
    (call-with-output-file loc
    #:exists 'append
    (lambda (output-port)
      (display border output-port)
      (newline output-port)
      ))
    (insertIn loc list 1))
   (else
    (call-with-output-file loc
    #:exists 'append
    (lambda (output-port)
      (display (car list) output-port)
      (newline output-port)
      ))
    (insertIn loc (cdr list) 1)
    
    )
   ))
    
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(define (chooseTeam);this function is for make team
  (newline)
  (display "Please follow all instructions below given with the folder")
  (newline)
  (display "Please,insert name of persons separated by comma each to make a team:")
  (newline)
  (define teamN (read-line))
  (set! team (string-split teamN ","))
  (display "How many persons in a team :")
  (newline)
  (define splitN (string->number(read-line)))
  (cond
    ((or (not (number? splitN)) (> splitN (length team 0)))
     (display "Error we can not create team for ")
     (display splitN)
     (display " with ")
     (display (length team 0))
     (display " persons,please try again")
     (chooseTeam))
    (else
     (display "Those are yours teams :")
     (makeTeam splitN team '() 0)
    )
     ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (directionProgram choose);this function sent toward the choose made
  (cond
    ((equal? 1 choose)
      (display "-------------------------------------------------PLACES----------------------------------------------------")
      (choosePlaces)
      (newline)
      (quesSave)
      (newline)
      (display "--------------------------------------------------DONE-----------------------------------------------------")
     )
    ((equal? 2 choose)
      (display "-------------------------------------------------TEAMS-----------------------------------------------------")
     (chooseTeam)
      (newline)
      (quesSave)
      (newline)
      (display "--------------------------------------------------DONE-----------------------------------------------------")
     )
    (else
     (display "GOOD-BYE")
     )
    )
  )
(directionProgram programExec)
<<<<<<< HEAD
                                                                                                                                           
;------------------------------------------------------------------------------------------------------> I am L 
=======
                                                                                                                                           ;L

>>>>>>> a06779235e65ced37047a3d284c40c4b9a46d443
  
  



                     
