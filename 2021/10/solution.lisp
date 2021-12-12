
(defvar *lines*)

(setf *lines*  
  (with-open-file (stream "input.txt")
    (loop for line = (read-line stream nil)
          while line
          collect (coerce line 'list))))


(defparameter *par*
  '((#\( #\) 3 1)
    (#\[ #\] 57 2)
    (#\{ #\} 1197 3)
    (#\< #\> 25137 4)))


(defun opening-p (ch)
  (when (member ch (mapcar #'first *par*))
    t))

(defun closing-p (ch)
  (when (member ch (mapcar #'second *par*))
    t))

(defun matching (ch)
  (if (opening-p ch)
    (second (find ch *par* :key #'first))
    (first (find ch *par* :key #'second))))

(defun matching-p (ch1 ch2)
  (eql ch1 (matching ch2)))

(defun rate (ch)
  (if (opening-p ch)
    (third (find ch *par* :key #'first))
    (third (find ch *par* :key #'second))))

(defun parse (acc s)
  (if (null (car s))
    (list :missing acc)
    (let*
      ((s (copy-list s))  
       (h (pop s)))
      (cond
        ((opening-p h) (parse (cons h acc) s))
        ((closing-p h) (if (matching-p h (car acc))
                          (parse (cdr acc) s)
                          (return-from
                            parse
                            (list :error h))))))))
(apply #'+
  (mapcar
    (lambda (l)
      (rate (second l)))
    (remove-if-not
      (lambda (l)
        (eql :error (first l)))
      (mapcar (lambda (l)
                (parse '() l))
              *lines*))))


;-----------------------------

(defun rate2 (ch)
  (if (opening-p ch)
    (fourth (find ch *par* :key #'first))
    (fourth (find ch *par* :key #'second))))

(defun rate2-all (l)
  (let
      ((sum 0))
      (loop for ch in l
            do (setf sum (+ (* 5 sum)
                            (rate2 ch))))
      sum))

(let
  ((all (sort
          (mapcar
            (lambda (l)
              (rate2-all (second l)))
            (remove-if-not
              (lambda (l)
                (eql :missing (first l)))
              (mapcar (lambda (l)
                        (parse '() l))
                      *lines*)))
          #'>)))
  (elt all (/ (1- (length all)) 2)))
