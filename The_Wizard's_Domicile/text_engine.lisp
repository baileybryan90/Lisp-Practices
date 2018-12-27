;;;AUTHORS NOTE: I built this with guidance from
;;;a textbook called Land of Lisp. 


;;this is a look up table for the locations
;;(more accurately, an association list or alist)
(defparameter *nodes* '((living-room (you are in the living-room.
							a wizard is snoring loudly on the couch.))
						(garden (you are in a beautiful garden.
							there is a well in front of you.))
						(attic (you are in the attic.
							there is a giant welding torch in the corner.))))

(defparameter *edges* '((living-room (garden west door)
									 (attic upstairs ladder))
						(garden(living-room east door))
						(attic (living-room downstairs ladder))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
								   (bucket living-room)
								   (chain garden)
								   (frog garden)))

;;Boy I don't like that name. I'd like it to be 'current-room'
(defparameter *location* 'living-room)

;;List of user commands
(defparameter *allowed-commands* '(look walk pickup inventory))


;;by the way, this is an example of functional programming
;;This self contained style is easy to test in isolation
;;should look deeper into it later
(defun describe-location (location nodes)
	(cadr (assoc location nodes)))

;;note the data block with lisp code pieces in it
;;lispers call this 'quasiquoting', interesting
;;not too dissimilar to how commands and text can
;;both be inserted into a console stream
(defun describe-path (edge)
	`(there is a ,(caddr edge) going ,(cadr edge) from here.))
;btw, notice the charater in front, that is NOT a single quote.
;its a backquote, thats the key shared with ~. 
;Backquote is litterally a left quote, normally text editors
;just let you use right quotes on both sides.
;Bizarre that that there would be a backquote key considering that.
;I thought that was interesting.

;;describe every path from a given location
;;1: find relevant edges: (cdr(assoc location edges)) is found_edges
;;2: convert the edges found to their descriptions: mapcar #'describe-path (found_edges)
;;3: join the descriptions together:apply #'append(descriptions)
(defun describe-paths (location edges)
	(apply #'append (mapcar #'describe-path(cdr (assoc location edges)))))
;for reference: (mapcar #'function list) applies a given function to every member of 
;the given list
;Literally: MAP over the domain of the CAR of the list
;Note also that mapcar is an example of a higher order function
;that is, it is a function that can take functions as input
;more to note: #' is the symbol for the function operator
;its essentially a macro for (function given_function(list))
;you need it though in case you deal with symbols with the same names
;as functions (deeper, this allows seperate namespaces for symbols and functions)

;;Since its a nightmare to understand the difference 
;;between mapcar and apply, here are some stackoverflow
;;definitions
;;mapcar is a function that calls its first argument with each element of its second argument, in turn. The second argument must be a sequence.
;;apply calls function with arguments, just like funcall but with one difference: the last of arguments is a list of objects, which are passed to function as separate arguments, rather than a single list. We say that apply spreads this list so that each individual element becomes an argument.
;; so I guess mapcar means: map this function onto each element in the cdr
;; while using the car as the functions first argument.

(defun objects-at (loc objs obj-locs)
	(labels ((at-loc-p (obj)
			   (eq (cadr (assoc obj obj-locs)) loc)))
		(remove-if-not #'at-loc-p objs)))
;as you know, labels is the command for defining local functions
;this is done for the sake of encapsulation


(defun describe-objects (loc objs obj-loc)
	(labels ((describe-obj (obj)
				`(you see a ,obj on the floor.)))
	 (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))
;warning: careful about the autocomplete, '' cause me trouble when I meant '

;;This seems more like it should be 'look-room', should it be?
;;I describes everything about the current room
(defun look()
	(append (describe-location *location* *nodes*)
			(describe-paths *location* *edges*)
			(describe-objects *location* *objects* *object-locations*)))

;;(find thing space :key #'func') note the :key.
;;Thats an example of a special parameter.
;;more on this later
(defun walk(direction)
 (let ((next (find direction 
 				(cdr (assoc *location* *edges*))
 				:key #'cadr)))
 (if next
 	(progn (setf *location* (car next))
 			(look))
 	'(you cannot go that way))))

;;Picking stuff up
(defun pickup (object)
 (cond ((member object
 				(objects-at *location* *objects* *object-locations*))
 		(push (list object 'body) *object-locations*)
 		 `(you are now carrying the ,object))
 		(t `(you cannot get that.))))

 ;;Check inventory for items
 (defun inventory()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

 ;;Introductory iterface message
 ;;WARNING: you must enter data into read functions
 ;;enclosed by "", so instead of Bryan you must type "Bryan"
 ;;Anyway, I've used princ since only a human will see it
 ;;so disregard what I said about "" in this case.
(defun say-hello()
 	(princ "Please type your name: ")
 	(let ((name (read-line)))
 		(princ "Nice to meet you, ")
 		(princ name)))

;;Laughably easy custom repl
#||(defun game-repl ()
 	(loop (print(eval (read)))))
 ||#
;;;You can ctrl-shift-c and enter :a to escape loops
;;;such as this 
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  User Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;WARNING: Read and eval are HIGHLY exploitable
;;;If I weren't following the book I would not do this
;;;Both allow hackers to easy run malicious code even with
;;;this crude safeguards in place

;;custom read 
(defun game-read()
	(let ((cmd (read-from-string
				(concatenate 'string "(" (read-line) ")"))))
		(flet ((quote-it (x)
						(list 'quote x)))
			(cons (car cmd) (mapcar #'quote-it (cdr cmd))))))
;;;Remember, flet and labels are the same except
;;;labels allows recursion and flet does not
;;;I assume flet performs slightly better as a result
;;;I suppose intentionally disallowing recursion could be useful
;;;also

;;custom eval
(defun game-eval (sexp)
	(if (member (car sexp) *allowed-commands*)
		(eval sexp)
		'(I do not know that command.)))

;;Format symbol text to be readable
;;Pretty ridiculous application of recursion
;;I don't know how I would have designed that on my own.
(defun tweak-text (lst caps lit)
 (when lst
 (let ((item (car lst))
		(rest (cdr lst)))
 (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
 		((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
 		((eql item #\") (tweak-text rest caps (not lit)))
		(lit (cons item (tweak-text rest nil lit)))
 		(caps (cons (char-upcase item) (tweak-text rest nil lit)))
 		(t (cons (char-downcase item) (tweak-text rest nil nil)))))))

;;More human readable print
;; lots of new functions to look into if I have the time.
(defun game-print (lst)
 (princ (coerce (tweak-text (coerce (string-trim "() "
 												  (prin1-to-string lst))
									'list)
							t
 							nil)
				'string))
 (fresh-line))

;;better game repl
(defun game-repl()
	(let ((cmd (game-read)))
		(unless (eq (car cmd) 'quit)
			(game-print (game-eval cmd))
			(game-repl))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Graph Generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;convert characters that are illegal in DOT format into
;;underscores
;;params: replacement, predicate, subject
(defun dot-name (exp)
	(substitute-if #\_ (complement #'alphanumericp)(prin1-to-string exp)))
			;replacement  ;test                    ;subject
;;;note that substitute-if-not could have been used but
;;;according to the book '-if-not' functions are deprecated
;;;from ANSI Common Lisp. That seems insane to me, but I'll
;;;play along


(defparameter *max-label-length* 30)

;;adding labels to graph nodes
(defun dot-label (exp)
	(if exp
		(let ((s (write-to-string exp :pretty nil)))
			(if (> (length s) *max-label-length*)
				(concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
				s))
		""))

;;generating DOT information for the nodes
(defun nodes->dot (nodes)
	(mapc (lambda (node)
			(fresh-line)
			(princ (dot-name (car node)))
			(princ "[label=\"")
			(princ (dot-label node))
			(princ "\"];"))
		nodes))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;f