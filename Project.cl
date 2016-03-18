;;Joe Sheppard
;;CSC 358 Final Project
;;Report and Usage instructions attached



;;MAIN METHOD FOR COMPUTER TAKING THE FIRST TURN
;;this creates a local variable state, and initalizes it to the list
;;representing the empty state.
;;it then loops over multiple turns, using mod to determine whos turn it 
;;should be. At the start of each turn, it checks if there is a winner 
;;and the game should be ended. If Not, it calls either player-turn
;;or comp-turn to start the appropriate turn.
(defun main-comp-first ()
  (let ((state '((0 0 0)(0 0 0)(0 0 0))))
  (loop for turn from 1 to 10
      when (not (null (check-winner state)))
      do (progn (format t "GAME!!! ~%") (return))
      when (zerop (mod turn 2))
      do (player-turn state)
      when (not (zerop (mod turn 2)))
      do (comp-turn state))))


;;Identical to above, except the player is allowed to make the first move.
(defun main-player-first ()
  (let ((state '((0 0 0)(0 0 0)(0 0 0))))
  (loop for turn from 0 to 10
      when (not (null (check-winner state)))
      do (progn (format t "GAME!!! ~%") (return))
      when (zerop (mod turn 2))
      do (player-turn state)
      when (not (zerop (mod turn 2)))
      do (comp-turn state))))


;;the function that controls the player's turn. Displays the current
;;board state, an input key, and a message telling the player it is
;;his turn. It then waits for an input of the players move, and checks 
;;to see if such input is a legal move. If it is legal, it upates the board 
;;state accordingly. If it is not, it notifies the player, and restarts the
;;turn, waiting for another move.
(defun player-turn (state)
  (progn 
    (format t "~%")
    (format t "YOUR TURN! ~%")
    (display-board state)
    (format t "~%")
    (format t "KEY: ~%")
    (display-key)
    (let ((input (read)))
      (if (zerop (getAt state input))
          (update-state state input -1)
          (progn (format t "NOT LEGAL MOVE ~%")
                 (player-turn state))))))

;;this is the function that controls the computer's turn, and starts
;;the minimax algorithm. It executes the algorithm on the current board
;;state, and updates the board state and displays the updated board
;;reflecting the move the computer has selected.
(defun comp-turn (state)
  (progn (update-state state (determine-move state (cadr (mmax state))) 1)
         (display-board state)))

;;The half of the Minimax algorithm that represents the selection of move
;;for the computer to make. All the successors of the current state are
;;generated, and the other half of the algorithm is called on them. After
;;this is done, the successor state that returns the max value is the best
;;move for the computer to make at this point. At the beginning of the
;;function, the algorithm checks if the generation of successor states
;;have led to an end state via the check-winner function. This is the
;;terminating case for the alogirthm's recursion.
(defun mmax (state)
  (if (not (null (check-winner state)))
      (list (check-winner state) state)
    (loop with toReturn = nil
        with max = -10
        with temp = 0
        for s in (generate-suc state 1)
        do (setf temp (car (mmin s)))
        when (< max temp)
        do (setf toReturn s)
        when (< max temp)
        do (setf max temp)
        finally (return (list max toReturn)))))
          

;;similar to the above function, but instead looks for the minimum of the 
;;results of the mmax function of the successors. In a sense, this represents
;;the computer predicting the movehis oppent will make, assuming he 
;;is logical.
(defun mmin (state)
  (if (not (null (check-winner state)))
      (list (check-winner state) state)
    (loop with toReturn = nil
        with min = 10
        with temp = 0
        for s in (generate-suc state -1)
        do (setf temp (car (mmax s)))
        when (> min temp)
        do (setf toReturn s)
        when (> min temp)
        do (setf min temp)
        finally (return (list min toReturn)))))


;;since the minimax algorithm returns the state that the computer has
;;decided on, this function takes that state and returns the proper move,
;;used to update the general board state with the computer's move.
;;state1 is the inital state, state2 is the new state
(defun determine-move (state1 state2)
  (loop for i from 1 to 9
      when (not (equal (getAt state1 i)
                       (getAt state2 i)))
       do (return i)))


;;i is the mark to make on the state. 1 for computer (X) -1 for player (O)
;;updates the board state with a passed in value and location
(defun update-state (state spot i)
  (cond
   ((equal spot 1) (setf (caar state) i))
   ((equal spot 2) (setf (cadar state) i))
   ((equal spot 3) (setf (caddar state) i))
   ((equal spot 4) (setf (caadr state) i))
   ((equal spot 5) (setf (cadadr state) i))
   ((equal spot 6) (setf (third (second state)) i))
   ((equal spot 7) (setf (car (third state)) i))
   ((equal spot 8) (setf (second (third state)) i))
   ((equal spot 9) (setf (third (third state)) i))
   (t Nil)))



;;similar to above, instead of updating the board state, it returns 
;;the symbol representing the passed location. 
(defun getAt (state index)
   (cond
   ((equal index 1) (caar state))
   ((equal index 2) (cadar state))
   ((equal index 3) (caddar state))
   ((equal index 4) (caadr state))
   ((equal index 5) (cadadr state))
   ((equal index 6) (third (second state)))
   ((equal index 7) (car (third state)))
   ((equal index 8) (second (third state)))
   ((equal index 9) (third (third state)))
   (t Nil)))


;;generates the sucessor functions for a given state,
;;the turn parameters determines who will be making the move. 
;;1 for X (comp) -1 for player (O)
(defun generate-suc (state turn)
  (loop
    for i from 1 to 9
      when (zerop (getAt state i))
      collect 
        (let ((newstate (copy-tree state)))
          (progn (update-state newstate i turn) (values))
          newstate)))
    
    


;;displays an input key for the human
(defun display-key ()
  (format t " 1 | 2 | 3 ~%")
  (format t "---|---|---~%")
  (format t " 4 | 5 | 6 ~%")
  (format t "---|---|---~%")
  (format t " 7 | 8 | 9 ~%"))


;;displays a representation of the board given a state
(defun display-board (state)
  (format t " ~c | ~c | ~c ~%" (num2char (caar state)) (num2char (cadar state))  (num2char (caddar state)))
  (format t "---|---|---~%")
  (format t " ~c | ~c | ~c ~%" (num2char (caadr state)) (num2char (cadadr state)) (num2char (third (second state))))
  (format t "---|---|---~%")
  (format t " ~c | ~c | ~c ~%" (num2char (car (third state)))  (num2char (second (third state))) (num2char (third (third state))))
  (format t "~%"))


;;this is a helper function for display purposes. Marks in the board state
;;are represented as 1, 0, or -1. This function transforms them into the 
;;characters that are to be displayed.
(defun num2char (num)
  (cond ((> num 0) #\X)
        ((< num 0) #\O)
        (t #\ )))



;;is called to check if there is a winner given a board state.
;;first checks for a draw, and checks all the possible ways one can win 
;;the game. .1 represents a draw, 1 represents a computer victory,
;;and -1 represents a human victory. Nil is returns if there is no winner
;;and the game has not yet ended in a draw
(defun check-winner (state)
  (if (check-draw state) .1
  (if (not (null (check-winner-horizontal state)))
      (check-winner-horizontal state)
    (if (not (null (check-winner-vertical state)))
        (check-winner-vertical state)
      (check-winner-diag state)))))


;;this function checks if the given state is a draw. That is, if all
;;the spaces are filled with a mark.
(defun check-draw (state)
  (loop
    for i from 1 to 9
    when (zerop (getAt state i))
    do (return nil)
    finally (return t)))



;;the horizontal check is perhaps the easiest to do. Since each row is
;;a sublist in state, it checks if any of the sublists have identical
;;non-zero values.
(defun check-winner-horizontal (state)
  (cond
   ((null state) nil)
   ((null (check-equal (car state))) (check-winner-horizontal (cdr state)))
   (t (check-equal (car state)))))


(defun check-winner-vertical (state)
  (loop for i from 0 to 2
      when
        (not (null (check-equal (list 
                                (nth i (first state))
                                (nth i (second state))
                                (nth i (third state))))))
      do (return (check-equal (list 
                              (nth i (first state))
                              (nth i (second state))
                              (nth i (third state)))))
      finally
        (return nil)))




(defun check-winner-diag (state)
  (if 
      (not (null (check-equal (list
                              (first (first  state))
                              (second (second state))
                               (third (third state))))))
      
      (check-equal         (list
                            (first (first  state))
                            (second (second state))
                             (third (third state))))
    
    (check-equal          (list
                           (third (first state))
                           (second (second state))
                           (first (third state))))))
                                  


;;checks if all elements(integers) of a parameter, lis are equal, and not zero
;;and if they are, return that number, and return nil if not
(defun check-equal (lis)
  ( loop with x = (car lis)
      for e in lis
      when (not (equal x e))
       do (return nil)
      finally
        (if (equal x 0) (return nil) (return x))))
        
        

    
    
 