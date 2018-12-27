;;;This guess the number game doesn't really make sense
;;;outside the REPL does it?

;defparamter is specifically for defining globals
(defparameter *small* 1)
(defparameter *big* 100)

;(ash (target) (direction)): arithmetic shift.
;1 to shift right one, -1 to shift left one
;defun is for defining global functions
(defun guess-my-number ()
    (ash (+ *small* *big*) -1))
    
;(setf (target) (newform)): setf means 'set form' I think    
(defun smaller()
    (setf *big* (1- (guess-my-number)))
    (guess-my-number))
    
(defun bigger()
    (setf *small* (1+ (guess-my-number)))
    (guess-my-number))

(defun start-over ()
    (defparameter *small* 1)
    (defparameter *big* 100)
    (guess-my-number))
    
;defining local variables
(let ((a 5)
      (b 6)
      (c 7))
    (+ a b))

;defining local functions
(flet ((f (n)
        (+ n 10)))
 (f 5))

;defining multiple local functions at once
(flet ((f (n)
        (+ n 10))
    (g (n)
        (- n 3)))
(g (f 5)))

;defining functions and making them available for other functions
;;IMPORTANT this is what allows functions to call theselves!!
(labels ((a (n)
            (+ n 5))
        (b (n)

        (+ (a n) 6)))
 (b 10))

#||  
(print *small*)
(print *big*)

(print (guess-my-number))
||#