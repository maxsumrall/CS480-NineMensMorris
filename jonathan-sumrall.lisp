
;(setf cc '(PLAY-GAME #'JONATHAN-SUMRALL:MAKE-COMPUTER-MOVE #'JONATHAN-SUMRALL:MAKE-COMPUTER-MOVE))

;;;; ASSIGNMENT 5
;;;;
;;;; Nine-Men's Morris Player
;;;; Due TUESDAY, DECEMBER 6.  I may push this date to somewhat later (we'll see).
;;;;
;;;;
;;;; You will provide the TA a single file (a revised version of this one).
;;;; You'll write the heuristic board evaluation function and the alpha-beta searcher
;;;; for a NINE MEN'S MORRIS game.  If you don't know how to play Nine Men's Morris, and can't
;;;; find stuff online, get ahold of me.
;;;;
;;;;
;;;;
;;;; HOW TO USE THIS TEMPLATE FILE
;;;;
;;;; This is the file you will provide to the TA.  It contains your project-specific
;;;; code.  Don't bother providing the 9mm.lisp file -- we have it, thanks.
;;;;
;;;; I'm providing you with one function that ordinarily I ask students to write:
;;;; the MAKE-COMPUTER-MOVE function.  This function takes a game state and generates
;;;; all one-move-away states from there.  It then calls ALPHA-BETA on those states
;;;; returns the new game state which got the highest alpha-beta score.  Piece of
;;;; cake.  Like all the lambda expressions?  :-)
;;;;
;;;; You will write:
;;;;
;;;; 1. The ALPHA-BETA function.  This is a straightforward implementation of the
;;;;    function described in the lecture notes.
;;;;
;;;; 2. The EVALUATE function.  Or more specifically, you'll fill out one part of
;;;;    it (the progn block shown).  Here you'll provide an intelligent heuristic
;;;;    evaluator.  Right now the evaluator provided ("0") literally says that
;;;;    all unfinished games are draws.  That's pretty stupid -- surely you can
;;;;    do better than that!  You're welcome, and perhaps encouraged, to write your
;;;;    own separate function(s) and call it from within the EVALUATE function so as
;;;;    to keep the EVALUATE function code clean.  Keep whatever functions and
;;;;    auxillary code to within this file please.
;;;;
;;;; I'd very strongly suggest examining all of the 9mm.lisp file as it may contain 
;;;; some valuable functions and constants that you might want to know about.
;;;;
;;;; Your code will not only be in this file but will be in its own package as well.
;;;; Your package name will be :firstname-lastname
;;;; For example, the package name for MY package is called :sean-luke
;;;; will change the text :REPLACEME with your :firstname-lastname package name
;;;; in the TWO places it appears in this file.
;;;;
;;;; You will then name your file "firstname-lastname.lisp".  For example, I would
;;;; name my own file "sean-luke.lisp"
;;;;
;;;;
;;;; RUNNING THE CODE
;;;;
;;;; You'll want to compile your code first, like I do to my code in this example:
;;;;
;;;; (load "9mm")
;;;; (compile-file "9mm.lisp")
;;;; (load "9mm")
;;;; (load "sean-luke")
;;;; (compile-file "sean-luke.lisp")
;;;; (load "sean-luke")
;;;;
;;;; Now we're ready to go.  If you want to just do a quick human-human game, you
;;;; don't need to compile and load your own file, just the 9mm.lisp file.
;;;;
;;;; You can run a simple human-against-human example like this:
;;;;
;;;; (play-human-human-game)
;;;;
;;;; You can run a simple human-against-computer example like this (replace sean-luke
;;;; with your own package name in this example of course)
;;;;
;;;; (play-human-computer-game #'sean-luke:make-computer-move t)  
;;;;
;;;; ...or if you wish the computer to go first,
;;;;
;;;; (play-human-computer-game #'sean-luke:make-computer-move nil)
;;;;
;;;; You can play a head-to-head game against two computer players (loading both of course)
;;;; like this:
;;;;
;;;; (play-game #'sean-luke:make-computer-move #'keith-sullivan:make-computer-move)
;;;;
;;;; Note that for all three of these functions (play-human-human-game, play-human-computer-game,
;;;; play-game) there are various optional keywords, including the search depth, whether boards
;;;; are printed, etc.
;;;;
;;;;
;;;; IMPORTANT NOTE ABOUT NINE MEN'S MORRIS
;;;;
;;;; A Nine Men's Morris turn works like this: first you either add or move a piece
;;;; depending on the current stage of the agem.  Then if you have created a "mill" (three in
;;;; a row) as a result of your move, you get to remove an opponent's piece.  This means that
;;;; there are really up to two "turns" in a person's turn.
;;;;
;;;; The play-computer-move and play-human-move will do both parts automatically.  In the
;;;; alpha-beta searcher you should treat these as separate turns (that's how the MOVES function
;;;; works).  Thus if you call MOVES on a game, you'll get boards, some of which result in mills.
;;;; If you call moves on THOSE boards (the ones with mills), you'll get further boards with the
;;;; opponent's piece removed.  this means you can't rely on the ply to determine who's turn it is.
;;;; Rather, use the SITUATION function to determine whose turn it is and what they're supposed
;;;; to be doing (or the TURN funtion to just determine whose turn it is).
;;;;
;;;; Another important note: a player loses if (1) he can't move or (2) the number of pieces he
;;;; has drops below 2 and so he can no longer possibly make a mill and thus have a shot at winning
;;;; the game.
;;;;
;;;;
;;;;
;;;; TESTING POLICY
;;;;
;;;; Plagiarism rules still apply (no code given, no code borrowed from anyone or any source
;;;; except the TA and the Professor, do not lay eyes on other people's code anywhere, including
;;;; online).  However they are softened in the following fashion:
;;;;
;;;;    - You are strongly encouraged to play against each other as long as neither of you
;;;;      looks at each others' code nor discusses your alpha-beta code.  Go on the forum and
;;;;      ask for partners.
;;;;
;;;;    - You ARE allowed to discuss, in general and highly abstract terms, your own approach
;;;;      to the EVALUATE function.  Let a thousand heuristics bloom!  You are also welcome to
;;;;      go online and get ideas about how to do the EVALUATE function as long as you do NOT
;;;;      read code.  I'm just as good as the next person -- perhaps even better -- at gathering
;;;;      every scrap of nine men's morris code on the web and comparing your code against it.
;;;;
;;;;    - I will eventually post a wfasl file of my implementation of alpha-beta, plus the "stupid"
;;;;      evaluator, which you can test against.  I may post a smarter evaluator later.  You
;;;;      ungzip this file on osf1, then load it with (load "sean-luke") and it should work fine.
;;;;
;;;;
;;;; OTHER POLICIES
;;;;
;;;; - This code may have, and almost certainly has, bugs.  I'll post revisions as needed.
;;;;
;;;; - Near the end of the exam we will have a single-elimination tournament competition.
;;;;      The winner of the competition gets to play against the professor's "better" evaluator
;;;;      (assuming I've written one by then) and ALSO receives an improvement in his overall
;;;;      course grade.  Runners up may receive something smaller: at least a candy bar, perhaps
;;;;      a better incentive.
;;;;
;;;; - If your evaluator is too slow, you will be penalized with a cut in your search depth.
;;;;   Thus it's in your best interest to have as good an evaluator/depth combination as possible.
;;;;   Note that it's often the case that evaluators that are good on ODD depths are bad on EVEN
;;;;   depths (or vice versa!).  I won't say what depth will be used in the tournament.
;;;;
;;;; - To make your code run as fast as possible, I strongly suggest you read the code optimization
;;;;   chapter in the Lisp book.
;;;;
;;;; Good luck!
;;;;
;;;;


(defpackage :jonathan-sumrall 
  (:use "COMMON-LISP" "COMMON-LISP-USER")
  (:export "MAKE-COMPUTER-MOVE"))

(in-package :jonathan-sumrall)


(defun alpha-beta (game current-depth max-depth
		   is-maxs-turn-p expand terminal-p evaluate
		   alpha beta)

  (if (or (funcall terminal-p game) (> current-depth max-depth))
      (funcall evaluate game is-maxs-turn-p)
      ;;else, we do more
      (if (funcall is-maxs-turn-p game)
	  ;;;;;is max's turn
	  (progn
	  (loop for child in (funcall expand game) until (>= alpha beta) do
	      (setf alpha (max alpha (alpha-beta child (incf current-depth) max-depth is-maxs-turn-p expand terminal-p evaluate alpha beta))))
	  (if (>= alpha beta) beta alpha))
	  ;;;;;is min's turn
	  (progn
	    (loop for child in (funcall expand game) until (>= alpha beta) do
	      (setf beta (min beta (alpha-beta child (incf current-depth) max-depth is-maxs-turn-p expand terminal-p evaluate alpha beta))))
	    (if (>= alpha beta) alpha beta)))))

(defun evaluate (game is-maxs-turn-p)
  "Returns an evaluation, between min-wins and max-wins inclusive, for the game.
is-maxs-turn-p is a function which, when called and passed the game, returns true
if it's max's turn to play."
  (let ((end (game-over game)))
    (if (null end) ;; game not over yet

	(let* (
	       (finalScore 0)
	      (numMine 0)
	      (numTheirs 0)
	      (numOfMyMills 0)
	      (numOfTheirMills 0)
	      (mineAdjacent 0)
	      (myColor (if (funcall is-maxs-turn-p game) (turn game) (if (= (turn game) red) red black)))
	      (theirColor (if (= myColor red) black red))
	      )
	  ;;; IMPLEMENT ME
	  ;; in this block, do your code which returns a heuristic
	  ;; evaluation of the system.  Feel free to create an outside function
	  ;; and call it if you don't want all the code here.
	  (loop for place from 0 to 23 do
	      (if (= (pos game place) myColor) (incf numMine))

	       (if (= (pos game place) theirColor) (incf numTheirs))

	       (loop for each in (mills place) do
		    (if (mill-filled-p myColor each game) (incf numOfMyMills))
		    (if (mill-filled-p theirColor each game) (incf numOfTheirMills)))

	       (if (or (= (pos game place) myColor) (= (pos game place) empty))
		   (loop for each in (adjacent-positions place) do
			(if (= (pos game each) myColor) 
			    (progn
			    (incf mineAdjacent))))))
	  
	  
	  (decf finalScore (* 6 numTheirs))
	  (incf finalScore (* 1.5 numMine))
	  (incf finalScore (* 1.5 numOfMyMills))
	  (decf finalScore (* 2 numOfTheirMills))
	  (incf finalScore (* 2 mineAdjacent))
	     finalScore)

	  ;;; END IMPLEMENTATION

	  


	(if (= 0 end)  ;; game is a draw
	    0

	    ;; else, the game is over but not a draw.  Return its value.
	    ;;
	    ;; this is a deep-math way of saying the following logic:
	    ;;
	    ;; if black, then if turn=black and ismaxsturn, then max-wins
	    ;; if black, then if turn=red and ismaxsturn, then min-wins
	    ;; if black, then if turn=black and !ismaxsturn, then min-wins
	    ;; if black, then if turn=red and !ismaxsturn, then max-wins
	    ;; if red, then if turn=black and ismaxsturn, then min-wins
	    ;; if red, then if turn=red and ismaxsturn, then max-wins
	    ;; if red, then if turn=black and !ismaxsturn, then max-wins
	    ;; if red, then if turn=red and !ismaxsturn, then min-wins
	    ;;
	    ;; keep in mind that black=1, red=-1, max-wins=1000, red-wins = -1000
	    
	    ;; at this point someone won.
	    (* end (turn game) max-wins (if (funcall is-maxs-turn-p game) 1 -1))))))



;; I've decided to make this function available to you

(defun make-computer-move (game depth &optional verbose)
  "Makes a move automatically by trying alpha-beta on all possible moves and then picking
the one which had the highest value for max., and making that move and returning the new game."

;; Note that this adds/moves, then it optionally removes
  
  (let ((max (turn game)))
    (let ((move (max-element (moves game)
			     (lambda (g)
			       (alpha-beta g 0 depth 
					   (lambda (gm) (= (turn gm) max)) ;; is-maxs-turn-p
					   (lambda (gm) (moves gm)) ;; expand
					   (lambda (gm) (game-over gm)) ;; terminal-p
					   #'evaluate ;; evaluate
					   min-wins
					   max-wins)))))
      (when (or (= (situation move) black-removes)
	      (= (situation move) red-removes))  ;; need to go again
	  (when verbose (print-game move))
	  (setf move (max-element (moves move)
				  (lambda (g)
				    (alpha-beta g 0 depth 
						(lambda (gm) (= (turn gm) max)) ;; is-maxs-turn-p
						(lambda (gm) (moves gm)) ;; expand
						(lambda (gm) (game-over gm)) ;; terminal-p
						#'evaluate ;; evaluate
						min-wins
						max-wins)))))
      move)))
    

;; go back to cl-user
(in-package :cl-user)

