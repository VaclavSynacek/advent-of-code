
(defun spacifie (s ch)
  (substitute
    #\space
    ch
    s))

(defun read-line-of-numbers (s)
  (read-from-string
    (concatenate 'string
                  "(" s ")")))

(defvar *points*)
(defvar *folds*)

(with-open-file (stream "input.txt")
  (setf *points*
    (loop for line = (read-line stream nil)
          while (not (string= line ""))
          collect
            (read-line-of-numbers
              (spacifie
                line
                #\,))))
  (setf *folds*
    (loop for line = (read-line stream nil)
          while line
          collect
            (cdr (cdr (read-line-of-numbers
                        (spacifie
                          line
                          #\=)))))))

(defun after-fold (points y)
  (remove-if-not
    (lambda (point)
      (> (second point) y))
    points))

(defun before-fold (points y)
  (remove-if-not
    (lambda (point)
      (< (second point) y))
    points))

(defun fold-point (point y)
  (list
    (first point)
    (- (* 2 y) (second point))))


(defun fold-y (points y)
  (remove-duplicates
    (append
      (before-fold points y)
      (mapcar
        (lambda (p)
          (fold-point p y))
        (after-fold points y)))
    :test #'equalp))

(defun transpose-point (p)
  (list (second p) (first p)))

(defun transpose (points)
  (mapcar #'transpose-point points))

(defun fold-x (points x)
  (transpose (fold-y (transpose points) x)))

(defun fold-all (points folds)
  (let
    ((points points))
    (loop for f in folds
          do (setf points
                   (if (eq (first f) 'x)
                      (fold-x points (second f))
                      (fold-y points (second f)))))
    points))
                

(length (fold-all *points* (list (first *folds*))))

;----------------------------------------

(defvar *result*)

(setf *result*
  (fold-all *points* *folds*))


(loop for y from 0 to (apply #'max (mapcar #'second *result*))
  do (progn
       (loop for x from 0 to (apply #'max (mapcar #'first *result*))
          do (format t
                     (if (member (list x y) *result* :test #'equalp)
                       "#"
                       " ")))
       (format t "~%")))
