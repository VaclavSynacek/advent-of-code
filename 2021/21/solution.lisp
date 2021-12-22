(declaim (optimize (speed 3) (safety 0) (debug 0)))

(ql:quickload '(:alexandria :arrows :cl-zipper :str))

(use-package :arrows)

(defparameter +small-input+
  (list
    (list
      (list
       (list :player1 4 0)
       (list :player2 8 0))
      1)))

(defparameter +input+
  (list
    (list
      (list
       (list :player1 8 0)
       (list :player2 2 0))
      1)))


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

(defvar *results*)

(defun reset-results ()
  (setf *results*
    (list :player1 0 :player2 0)))

(defun add-wins (player times)
  (incf (getf *results* player) times))



(defvar *universe*)

(defun add-to-universe (variants)
  (loop for v in variants
        for state = (first v)
        for times = (second v)
        do (incf
             (gethash state *universe* 0)
             times)))

(defun init-universe (variants)
  (setf *universe*
    (make-hash-table :test #'equalp))
  (add-to-universe variants)
  (reset-results)
  (reset-dice))

(defun pop-from-universe ()
  (let
    ((min (loop for state being the hash-keys in *universe*
             using (hash-value times)
             minimize (third (first state)))))
    (loop for state being the hash-keys in *universe*
                 using (hash-value times)
                 do (when (= min (third (first state)))
                      (remhash state *universe*)
                      (return-from
                        pop-from-universe
                        (list state times))))))

(defun move (start n)
  (loop for i  = (+ start n) then (- i 10)
        while (> i 10)
        finally (return i)))


(defun all-dices ()
  (list
    (list
      1
      (apply #'+ (list (roll) (roll) (roll))))))


(defun play (init)
  (init-universe init)
  (loop for now = (pop-from-universe)
        while now
        do
          (let*
           ((state (first now))
            (times (second now))
            (player (first state))
            (all-dices (all-dices)))
           (loop for possible-outcome in all-dices
                 do
                   (let*
                    ((times   (* times (first possible-outcome)))
                     (dices   (second possible-outcome))
                     (player  (copy-tree player))
                     (new-pos (move
                                (second player)
                                dices)))
                    (setf (second player) new-pos)
                    (if (>=
                          (incf (third player) new-pos)
                          1000)
                        (add-wins (first player) times)
                        (add-to-universe
                          (list
                            (list
                             (list (second state) player)
                             times)))))))))

(play (copy-tree +input+))

*results*

