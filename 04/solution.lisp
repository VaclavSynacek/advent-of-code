
(defvar *random-numbers*)
(defvar *boards*)

(defun read-line-of-numbers (s)
  (read-from-string
    (concatenate 'string
                  "(" s ")")))

(with-open-file (stream "input.txt")
  (setf *random-numbers*
        (read-line-of-numbers
          (substitute
            #\space
            #\,
            (read-line stream nil))))
  (setf *boards*
        (loop for empty-line? = (read-line stream nil)
              while empty-line?
              collect (loop repeat 5
                            collect (read-line-of-numbers
                                      (read-line stream nil))))))


(defun mark (board num)
  (mapcar
    (lambda (line)
      (mapcar
        (lambda (n)
          (when (numberp n)
            (if (= num n)
              nil
              n)))
        line))
    board))


(defun row-wins? (board)
  (some
    (lambda (row)
      (every #'null row))
    board))
#|
(row-wins?
  '((1 1)
    (nil nil)))
|#

(defun transpose (board)
  (loop for i from 0 to 4
        collect (loop for row in board
                      collect (elt row i))))

(defun col-wins? (board)
  (row-wins? (transpose board)))

(defun wins? (board)
  (or
    (row-wins? board)
    (col-wins? board)))

(mark (first *boards*) 80)

*random-numbers*

(setf *last-number*
  (loop named turns
        for num in *random-numbers*
        do (progn
             (setf
               *boards*
               (mapcar
                 (lambda (b)
                   (mark b num))
                 *boards*))
             (when (some #'wins? *boards*)
               (return-from turns num)))))

(setf *winner*
      (first (remove-if-not
               #'wins?
               *boards*)))

(defun sum-board (board)
  (reduce #'+
     (mapcar
       (lambda (row)
         (reduce #'+
                 (remove nil row)))
       board)))

(reduce #'+ (remove nil '(nil 2 3 4)))

(* *last-number* (sum-board *winner*))

;;-----------------------

(with-open-file (stream "input.txt")
  (setf *random-numbers*
        (read-line-of-numbers
          (substitute
            #\space
            #\,
            (read-line stream nil))))
  (setf *boards*
        (loop for empty-line? = (read-line stream nil)
              while empty-line?
              collect (loop repeat 5
                            collect (read-line-of-numbers
                                      (read-line stream nil))))))

(defvar *last-number*)

(setf *last-number*
  (loop named turns
        for num in *random-numbers*
        do (progn
             (setf
               *boards*
               (remove-if
                 #'wins?
                 (mapcar
                  (lambda (b)
                    (mark b num))
                  *boards*)))
             (when (= 1 (length *boards*))
               (setf *last-board*
                     (first *boards*)))
             (format t "~a: ~a~%" num (length *boards*))
             (when (every #'wins? *boards*)
               (return-from turns num)))))

(* *last-number* (sum-board (mark *last-board* *last-number*)))
