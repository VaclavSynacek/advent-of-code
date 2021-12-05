
(defun spacifie (s ch)
  (substitute
    #\space
    ch
    s))

(defun read-line-of-numbers (s)
  (read-from-string
    (concatenate 'string
                  "(" s ")")))

(with-open-file (stream "input.txt")
  (setf *all-lines*
    (loop for line = (read-line stream nil)
          while line
          collect
            (read-line-of-numbers
              (spacifie
                (spacifie
                  (spacifie
                    line
                    #\,)
                  #\-)
                #\>)))))

(length *all-lines*)

; (apply #'max (mapcar (lambda (l) (apply #'max l)) *all-lines*))


(defun horizontal? (line)
  (= (first line) (third line)))

(defun vertical? (line)
  (= (second line) (fourth line)))

(defun diagonal? (line)
  (not
    (or
      (horizontal? line)
      (vertical? line))))

(setf *lines* (remove-if #'diagonal? *all-lines*))


(defparameter *sea* (make-array '(1000 1000) :initial-element 0))

(defun draw-vertical (l)
  (format t "drawing vertical ~a~%" l)
  (loop for x from (min (first l) (third l)) to (max (first l) (third l))
        do (incf (aref *sea* (second l) x)))
  (format t "sea afterwards~%")
  (print *sea*)
  (format t "~%"))

(defun draw-horizontal (l)
  (format t "drawing horizontal ~a~%" l)
  (loop for y from (min (second l) (fourth l)) to (max (second l) (fourth l))
        do (incf (aref *sea* y (first l))))
  (format t "sea afterwards~%")
  (print *sea*)
  (format t "~%"))

(loop for l in *lines*
      do (if (horizontal? l)
           (draw-horizontal l)
           (if (vertical? l)
             (draw-vertical l))))

(length (loop for i from 0 below (array-total-size *sea*)
             when (> (row-major-aref *sea* i) 1)
             collect 1))


;----------------------------------------

(defun x1 (l)
  (first l))
(defun x2 (l)
  (third l))
(defun y1 (l)
  (second l))
(defun y2 (l)
  (fourth l))

(defun swap-points (l)
  (list (x2 l) (y2 l) (x1 l) (y1 l)))

(defun normalize-left-to-right (l)
  (if (< (x2 l) (x1 l))
    (swap-points l)
    l))

(with-open-file (stream "input.txt")
  (setf *all-lines*
    (mapcar
      #'normalize-left-to-right
      (loop for line = (read-line stream nil)
           while line
           collect
             (read-line-of-numbers
               (spacifie
                 (spacifie
                   (spacifie
                     line
                     #\,)
                   #\-)
                 #\>))))))

(defparameter *sea* (make-array '(1000 1000) :initial-element 0))



(defun diagonal-up? (l)
  (and
    (diagonal? l)
    (> (y2 l) (y1 l)))) 

(defun diagonal-down? (l)
  (and
    (diagonal? l)
    (not (diagonal-up? l))))

(defun draw-diagonal-up (l)
  (format t "drawing diagonal up ~a~%" l)
  (loop for x from (x1 l) to (x2 l)
        for y from (y1 l) to (y2 l)
        do (incf (aref *sea* y x))))
  ;(format t "sea afterwards~%")
  ;(print *sea*)
  ;(format t "~%"))

(defun draw-diagonal-down (l)
  (format t "drawing diagonal down ~a~%" l)
  (loop for x from (x1 l) to (x2 l)
        for y from (y1 l) downto (y2 l)
        do (incf (aref *sea* y x))))
  ;(format t "sea afterwards~%")
  ;(print *sea*)
  ;(format t "~%"))


(loop for l in *all-lines*
      do (if (horizontal? l)
           (draw-horizontal l)
           (if (vertical? l)
             (draw-vertical l)
             (if (diagonal-up? l)
                (draw-diagonal-up l)
                (draw-diagonal-down l)))))

; *all-lines*

; *sea*

(length (loop for i from 0 below (array-total-size *sea*)
             when (> (row-major-aref *sea* i) 1)
             collect 1))
