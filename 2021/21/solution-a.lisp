(declaim (optimize (speed 3) (safety 0) (debug 0)))

(ql:quickload '(:alexandria :arrows :cl-zipper :str))

(use-package :arrows)

(defparameter +small-input+
  (list
    (list :player1 4 0)
    (list :player2 8 0)))

(defparameter +input+
  (list
    (list :player1 8 0)
    (list :player2 2 0)))

(defvar *dice-state*)
(defvar *roll-count*)

(defun reset-dice ()
  (setf *dice-state* 0)
  (setf *roll-count* 0))

(defun roll ()
  (incf *roll-count*)
  (incf *dice-state*)
  (if (= *dice-state* 101)
    (setf *dice-state* 1)
    *dice-state*))

(defun move (start n)
  (loop for i  = (+ start n) then (- i 10)
        while (> i 10)
        finally (return i)))


(defun play (state)
  (format t "called with ~a ~%" state)
  (let*
   ((player (first state))
    (dices (list (roll) (roll) (roll)))
    (new-pos (move
               (second player)
               (apply #'+ dices))))
   (format t "from ~a, rolled ~a, to ~a ~%" (second player) dices new-pos)
   (setf (second player) new-pos)
   (if (>=
         (incf (third player) new-pos)
         1000)
     (list player (second state))
     (play (list (second state) player)))))


(reset-dice)
(-> (play (copy-tree +input+))
    (second)
    (third)
    (* *roll-count*))

