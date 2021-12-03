
(defvar *input-numbers*)

(setf *input-numbers*  
  (with-open-file (stream "input.txt")
    (loop for line = (read-line stream nil)
          while line
          collect (read-from-string line))))

(defun increased (data)
  (count t
    (maplist
      (lambda (l)
        (when (cadr l)
          (< (car l) (cadr l))))
      data)))

(increased *input-numbers*)

(increased
  (maplist
    (lambda (l)
      (when (third l)
        (+ (first l) (second l) (third l))))
    *input-numbers*))
