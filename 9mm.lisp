(in-package :cl-user)

;;;;;;;;;;; UTILITIES

;; we use this to copy game boards
(defun copy-array (array)
  "Copies an Array"
  ;; from http://lemonodor.com/archives/000100.html
  (let ((dims (array-dimensions array)))
    (adjust-array
     (make-array dims :element-type (array-element-type array) :displaced-to array)
     dims)))


;; you might find this useful in your MAKE-COMPUTER-MOVE function (hint)
(defun max-element (elements value-function)
  "Returns the element from elements which, when passed into value-function,
returned the highest value.  If elements is nil, nil is returned.  In case
of ties, the later element is returned."
  (when elements
    (let ((probability-count 1)
	  (max-element (first elements))
	  (max-element-score (funcall value-function (first elements))))
      (dolist (element (rest elements))
	(let ((element-score (funcall value-function element)))
	  (cond ((> element-score max-element-score)
		 (setf probability-count 1)
		 (setf max-element element)
		 (setf max-element-score element-score))
		((= element-score max-element-score)
		 (incf probability-count)
		 (when (<= (random 1.0) (/ 1 probability-count))
		   (setf max-element element)
		   (setf max-element-score element-score))))))
      max-element)))


;;;;;;;;;;; CONSTANTS
;;; I could use defconstant but it always trips up studentsxh
;;; when compiling and then reloading, so they're global variables here.
;;; Treat them as constants.

(defparameter black 1 "A black piece, or the player 'black', or black's turn")
(defparameter red -1 "A red piece, or the player 'red', or red's turn")
(defparameter empty 0 "An empty location")
(defparameter black-removes 2 "Black's turn, and black gets to remove")
(defparameter red-removes -2 "Red's turn, and red gets to remove")

(defparameter num-pieces 9 "Number of pieces for each player")
(defparameter num-positions 24 "Number of positions on the board")

(defparameter max-wins 1000 "Alpha-beta score if max wins")
(defparameter min-wins -1000 "Alpha-beta score if min wins")

;;;;;;;;;;; THE GAME BOARD

;;; A GAME is defined as a list (board situation unplayed-red unplayed-black removed-red removed-black depth)
;;;
;;; where BOARD is an array of spaces as follows
;;; +--+--+     0,  1,  2
;;; |+-+-+|     3,  4,  5
;;; ||+++||     6,  7,  8
;;; +++ +++     9,  10,  11,  12,  13, 14
;;; ||+++||     15,  16,  17
;;; |+-+-+|     18,  19,  20
;;; +--+--+     21,  22,  23
;;;
;;; Each space is an integer, one of black, red, or empty
;;;
;;; Next, SITUATION is an integer, one of:
;;;
;;;     black                it's black's turn to either put a piece down (if unplayed-black > 0) or move a piece
;;;     red                  it's red's turn to either put a piece down (if unplayed-red > 0) or move a piece
;;;     black-removes        it's black's turn and he gets to remove a piece
;;;     red-removes          it's red's turn and he gets to remove a piece
;;;
;;; Next, UNPLAYED-RED, UNPLAYED-BLACK, REMOVED-RED, and REMOVED-BLACK are integers indicating how many
;;  red or black tokens have been played onto the board and how many have been permanently removed from
;;; the board so far.
;;;
;;; Next, DEPTH is the current depth of the board in the game tree search 

(defun board (game) "The game board as an array" (first game))
(defun (setf board) (val game) (setf (first game) val))

(defun situation (game) "The situation, one of black red black-removes red-moves" (second game))
(defun (setf situation) (val game) (setf (second game) val))
(defun turn (game) "Returns whose turn it is" (if (or (= (situation game) black) (= (situation game) black-removes)) black red))

(defun unplayed (game player) "The number of unplayed tokens for the player" (if (= player red) (third game) (fourth game)))
(defun (setf unplayed) (val game player) (if (= player red) (setf (third game) val) (setf (fourth game) val)))

(defun removed (game player) "The number of removed tokens for the player" (if (= player red) (fifth game) (sixth game)))
(defun (setf removed) (val game player) (if (= player red) (setf (fifth game) val) (setf (sixth game) val)))

(defun depth (game) "The game depth" (seventh game))
(defun (setf depth) (val game) (setf (seventh game) val))

(defun pos (game p) "The kind of token at the given position, one of black red empty" (aref (board game) p))
(defun (setf pos) (val game p) (setf (aref (board game) p) val))

(defun make-game ()
  (list (make-array `(,num-positions) :element-type 'fixnum :initial-element empty)
	black num-pieces num-pieces 0 0 0))

(defun copy-game (game)
  "Copies a game, board and all."
  (cons (copy-array (board game))
	(copy-list (rest game))))


(defparameter *adjacent-positions*
 (make-array `(,num-positions)
	      :initial-contents  '((1 9)         ;; 0
				   (0 2 4)       ;; 1
				   (1 14)        ;; 2
				   (10 4)        ;; 3
				   (1 3 5 7)     ;; 4
				   (4 13)        ;; 5
				   (11 7)        ;; 6
				   (4 6 8)       ;; 7
				   (7 12)        ;; 8
				   (0 10 21)     ;; 9
				   (3 9 11 18)   ;; 10
				   (6 10 15)     ;; 11
				   (8 13 17)     ;; 12
				   (5 12 14 20)  ;; 13
				   (2 13 23)     ;; 14
				   (11 16)       ;; 15
				   (15 19 17)    ;; 16
				   (12 16)       ;; 17
				   (10 19)       ;; 18
				   (16 18 20 22) ;; 19
				   (13 19)       ;; 20
				   (9 22)        ;; 21
				   (19 21 23)    ;; 22
				   (14 22))))    ;; 23

(defun adjacent-positions (p)
  "Returns a list of all the adjacent positions to the given position"
  (aref *adjacent-positions* p))

(defun adjacent-p (p1 p2)
  "Returns TRUE if p1 is adjacent to p2"
  (member p2 (adjacent-positions p1)))


;;;; MILLS
;;;;
;;;; A mill is three pieces in a row

(defparameter *mills*
 (make-array `(,num-positions)
	      :initial-contents  '(((0 1 2) (0 9 21))         ;; 0
				   ((0 1 2) (1 4 7))          ;; 1
				   ((0 1 2) (2 14 23))        ;; 2
				   ((3 4 5) (3 10 18))        ;; 3
				   ((3 4 5) (1 4 7))          ;; 4
				   ((3 4 5) (5 13 20))        ;; 5
				   ((6 7 8) (6 11 15))        ;; 6
				   ((6 7 8) (1 4 7))          ;; 7
				   ((6 7 8) (8 12 17))        ;; 8
				   ((9 10 11) (0 9 21))       ;; 9
				   ((9 10 11) (3 10 18))      ;; 10
				   ((9 10 11) (6 11 15))      ;; 11
				   ((12 13 14) (8 12 17))     ;; 12
				   ((12 13 14) (5 13 20))     ;; 13
				   ((12 13 14) (2 14 23))     ;; 14
				   ((15 16 17) (6 11 15))     ;; 15
				   ((15 16 17) (16 19 22))    ;; 16
				   ((15 16 17) (8 12 17))     ;; 17
				   ((18 19 20) (3 10 18))     ;; 18
				   ((18 19 20) (16 19 22))    ;; 19
				   ((18 19 20) (5 13 20))     ;; 20
				   ((21 22 23) (0 9 21))      ;; 21
				   ((21 22 23) (16 19 22))    ;; 22
				   ((21 22 23) (2 14 23)))))  ;; 23


(defun mills (p)
  "Returns all mills which intersect with the given position"
  (aref *mills* p))

(defun mill-filled-p (player mill game)
  "Returns TRUE if the given mill is all filled by the provided player"
  (and (= (pos game (first mill)) player)
       (= (pos game (second mill)) player)
       (= (pos game (third mill)) player)))
  
(defun mill-formed-p (player p game)
  "Returns TRUE if a mill was formed by the given player when he placed a piece or moved a piece to position p"
  (let ((m (mills p)))
    (or (mill-filled-p player (first m) game)
	(mill-filled-p player (second m) game))))


;;;;;;;;;;; MOVES

(defun moves (game &optional (count nil))
  "If count is nil (the default, then returns, as a list, all possible new game states resulting from moves which can be made.
If on the other hand count is TRUE, then returns the NUMBER of possible new game states."
  (let ((bag (if count 0 nil)))
    (cond ((= (situation game) black)       
	   (if (> (unplayed game black) 0)  
	       (dotimes (p num-positions)        ;; black's turn, putting a piece down
		 (if (= (pos game p) empty)
		     (if count (incf bag)
			 (let ((g (copy-game game)))
			   (setf (pos g p) black)
			   (decf (unplayed g black))
			   (if (mill-formed-p black p g)
			       (setf (situation g) black-removes)
			       (setf (situation g) red))
			   (push g bag)))))
	       
		      (dotimes (p num-positions)        ;; black's turn, moving a piece
			(if (= (pos game p) black)
			    (dolist (pp (adjacent-positions p))
			      (if (= (pos game pp) empty)
				  (if count (incf bag)
				      (let ((g (copy-game game)))
					(rotatef (pos g p) (pos g pp))
					(if (mill-formed-p black pp g)
					    (setf (situation g) black-removes)
					    (setf (situation g) red))
					(push g bag)))))))))

	  ((= (situation game) black-removes)  ;; black's turn, removing a piece of red's
	   (dotimes (p num-positions)
	     (if (= (pos game p) red)
		 (if count (incf bag)
		     (let ((g (copy-game game)))
		       (setf (pos g p) empty)
		       (setf (situation g) red)
		       (incf (removed g red))
		       (push g bag))))))
	   
	  ((= (situation game) red)       
	   (if (> (unplayed game red) 0)  
	       (dotimes (p num-positions)        ;; red's turn, putting a piece down
		 (if (= (pos game p) empty)
		     (if count (incf bag)
			 (let ((g (copy-game game)))
			   (setf (pos g p) red)
			   (decf (unplayed g red))
			   (if (mill-formed-p red p g)
			       (setf (situation g) red-removes)
			       (setf (situation g) black))
			   (push g bag)))))
	       
	       (dotimes (p num-positions)        ;; red's turn, moving a piece
		 (if (= (pos game p) red)
		     (dolist (pp (adjacent-positions p))
		       (if (= (pos game pp) empty)
			   (if count (incf bag)
			       (let ((g (copy-game game)))
				 (rotatef (pos g p) (pos g pp))
				 (if (mill-formed-p red pp g)
				     (setf (situation g) red-removes)
				     (setf (situation g) black))
				 (push g bag)))))))))
	  
	  ((= (situation game) red-removes)  ;; red's turn, removing a piece of black's
	   (dotimes (p num-positions)
	     (if (= (pos game p) black)
		 (if count (incf bag)
		     (let ((g (copy-game game)))
		       (setf (pos g p) empty)
		       (setf (situation g) black)
		       (incf (removed g black))
		       (push g bag)))))))
    bag))
	  
	  

;;;;;;;;;;; BOARD ANALYSIS
  
(defun game-over (game)
  "Returns 1 (black) if the game was won by black, -1 if the game was won by red,
0 if the game has drawn, and NIL if the game is NOT OVER YET"
  (if (>= (removed game black) (- num-pieces 2))  ;; black is out -- he cannot possibly win
      red
      (if (>= (removed game red) (- num-pieces 2))  ;; red is out -- he cannot possibly win
	  black
	  (if (= 0 (moves game t))  ;; no moves left to make.  only happens in red or black, not red-removes or black-removes
	      (if (= (situation game) red)   ;; red has no moves
		  black
		  red)
	      nil))))



(defun print-game (game)
  (apply #'format t "~%~a~%Black: ~a  Red: ~a~%~a--~a--~a    0  1  2~%|~a-~a-~a|    3  4  5~%||~a~a~a||    6  7  8~%~a~a~a ~a~a~a    9  10 11 12 13 14~%||~a~a~a||    15 16 17~%|~a-~a-~a|    18 19 20~%~a--~a--~a    21 22 23~%"
	 (cond ((game-over game) (if (= (game-over game) black) "Black wins" "Red wins"))
	       ((= (situation game) black) 
		(if (> (unplayed game black) 0) "Black to add" "Black to move"))
	       ((= (situation game) red) 
		(if (> (unplayed game red) 0) "Red to add" "Red to move"))
	       ((= (situation game) black-removes) "Black to remove")
	       ((= (situation game) red-removes) "Red to remove"))
	 (- num-pieces (removed game black)) (- num-pieces (removed game red))
	 (map 'list #'(lambda (elt) (if (= elt black) #\@ (if (= elt red) #\O #\+))) (board game))))


(defun flush-format (stream string &rest args)
  (apply #'format stream string args)
  (finish-output stream))

(defun make-human-move (game &optional depth verbose)  ;; depth is ignored -- see tournament.  verbose is also ignored
  (declare (ignore depth))
  (declare (ignore verbose))
  ;; first allow adding or moving
  (flush-format t "~%You are ~a" (if (= (situation game) black) "Black (@)" "Red (O)"))
  (let (mill-formed g)
    (if (> (unplayed game (situation game)) 0)  ;; adding
	(loop
	   (flush-format t "~%Place on -> ")
	   (let ((p (read)))
	     (when (or (eq p 'Q) (eq p 'q)) (format t "Quitting...") (break))
	     (when (and (numberp p) (>= p 0) (< p num-positions) (= (pos game p) empty))
	       (setf g (copy-game game))
	       (setf (pos g p) (situation game))
	       (decf (unplayed g (situation game)))
	       (setf mill-formed (mill-formed-p (situation game) p g))
	       (return)))
	   (flush-format t "Come again?   If you want to quit, type   q~%"))
	(loop                                 ;; moving
	   (flush-format t "~%Move from -> ")
	   (let ((p (read)))
	     (when (or (eq p 'Q) (eq p 'q)) (format t "Quitting...") (break))
	     (when (and (numberp p) (>= p 0) (< p num-positions) (= (pos game p) (situation game)))  ;; I can move from there
	       (flush-format t "Move to -> ")
	       (let ((p2 (read)))
		 (when (or (eq p 'Q) (eq p 'q)) (format t "Quitting...") (break))
		 (when (and (numberp p2)
			    (>= p2 0) 
			    (< p2 num-positions) 
			    (= (pos game p2) empty)
			    (adjacent-p p p2))
		   (setf g (copy-game game))
		   (setf (pos g p) empty)
		   (setf (pos g p2) (situation game))
		   (setf mill-formed (mill-formed-p (situation game) p2 g))
		   (return)))))
	   (flush-format t "Come again?   If you want to quit, type   q~%")))
    (when mill-formed                       ;; allow removal
      (loop
	 (flush-format t "~%Remove -> ")
	 (let ((p (read)))
	   (when (or (eq p 'Q) (eq p 'q)) (format t "Quitting...") (break))
	   (when (and (numberp p) (>= p 0) (< p num-positions) (= (pos g p) (if (= (situation g) black) red black)))
	     (setf g (copy-game g))
	     (setf (pos g p) empty)
	     (incf (removed g (if (= (situation g) black) red black)))
	     (return)))
	 (flush-format t "Come again?   If you want to quit, type   q~%")))
    (setf (situation g) (if (= (situation g) black) red black))
    g))



(defun play-game (black-func red-func &key (black-depth 5) (red-depth nil) (verbose t) (max-turns 100))
  "Plays a game between the two functions, passing in maximum
depths for each.  Optionally prints the board each move.  If red-depth is not
supplied, it is assumed to be equal to black-depth."
  (unless red-depth (setf red-depth black-depth))
  (let ((game (make-game)) (turn 0))
    (loop
       (incf turn)
       (when (> turn max-turns)
	 (return-from play-game 0))
       (when verbose (print-game game))
       (let ((over (game-over game)))
	 (when over
	   (when verbose (format t (cond ((equalp over black) "~%~%Black Wins")
					 ((equalp over red) "~%~%Red Wins")
					 (t "~%~%Draw ~a ~a ~a" red black over))))
	   (return-from play-game over)))
       (setf game (funcall black-func game black-depth verbose))
       (when verbose (print-game game))
       (let ((over (game-over game)))
	 (when over
	   (when verbose (format t (cond ((equalp over black) "~%~%Black Wins")
					 ((equalp over red) "~%~%Red Wins")
					 (t "~%~%Draw ~a ~a ~a" red black over))))
           (return-from play-game over)))
       (setf game (funcall red-func game red-depth verbose)))))

(defun play-human-human-game ()
  (play-game #'make-human-move #'make-human-move
		   :verbose t))

(defun play-human-computer-game (func human-is-black &key (computer-depth 4))
  (play-game (if human-is-black #'make-human-move func)
	     (if human-is-black func #'make-human-move)
	     :verbose t
	     :black-depth computer-depth
	     :red-depth computer-depth))

(defun play-tournament (black-func red-func &key (black-depth 4) (red-depth 4) (verbose t))
  (let (round-1 round-2)

    (format t "~%~%~%~%-----------------------~a PLAYS AS BLACK-----------------------~%~%~%~%" black-func)
    (terpri)
    (setf round-1 (play-game black-func red-func :black-depth black-depth :red-depth red-depth :verbose verbose))
    (format t "~%~%~%~%------------------------~a PLAYS AS BLACK------------------------~%~%~%~%" red-func)
    (terpri)
    (setf round-2 (play-game red-func black-func :black-depth red-depth :red-depth black-depth :verbose verbose))
    
    (format t "~%~%~%~%------------------------ ~a --------------------------~%"
	    (if (= round-1 round-2) "TIE!"
		(format t "~a WINS!" (if (= round-1 black) black-func red-func))))
    nil))

;;; export some symbols so the individual evaluation packages can see them (for example, :sean-luke)

(export '(copy-array max-element black red empty black-removes red-removes num-pieces num-positions
	  max-wins min-wins turn
	  board situation unplayed removed depth pos make-game copy-game 
	  adjacent-positions adjacent-p mills mill-filled-p mill-formed-p
	  moves game-over print-game make-human-move 
	  play-game play-human-game play-human-computer-game play-tournament))



