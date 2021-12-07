
(defvar *lines*)

(setf *lines*  
  (with-open-file (stream "big-input.txt")
    (loop for line = (read-line stream nil)
          while line
          collect line)))


(defun parse-line-inner (symbols line)
  (let
    ((line (copy-list line)))
    (if line
      (let*
        ((h (pop line))
         (next (case h
                 ((#\w #\e) (string h))
                 (otherwise (coerce (list h (pop line)) 'string)))))
        (parse-line-inner (cons (read-from-string next) symbols) line))
      symbols)))

(defun parse-line (line-string)
  (parse-line-inner '() (coerce line-string 'list)))

#|

(parse-line (first *lines*))

|#

(defvar *parsed*)

(setf *parsed* (mapcar #'parse-line *lines*))

(defvar *x* 0)
(defvar *y* 0)

(defun w ()
  (decf *x*))
(defun e ()
 (incf *x*)) 
(defun nw ()
  (when (oddp *y*)
    (decf *x*))
  (decf *y*))
(defun ne ()
  (when (evenp *y*)
    (incf *x*))
 (decf *y*)) 
(defun sw ()
  (when (oddp *y*)
    (decf *x*))
  (incf *y*))
(defun se ()
  (when (evenp *y*)
    (incf *x*))
 (incf *y*)) 

(defun eval-line (line)
  (let
    ((*x* 0)
     (*y* 0))
    (mapc (lambda (c) (funcall c)) line)
    (list *x* *y*)))

#|

(first *parsed*)

(eval-line (parse-line "esew"))

(eval-line (parse-line "nwwswee"))

(mapcar #'eval-line *parsed*)

|#

(defun occurrences (lst)
  (let ((table (make-hash-table :test #'equalp)))
    (loop for e in lst
          do (incf (gethash e table 0)))
    (loop for k being the hash-key of table
          using (hash-value v)
          collect (cons k v))))                         

(defvar *initial-blacks*)

(setf *initial-blacks*
  (remove-if
   #'evenp
   (occurrences
     (mapcar #'eval-line *parsed*))
   :key #'cdr))

(length *initial-blacks*)

;----------------------

(setf *initial-blacks*
      (mapcar #'first *initial-blacks*))

(defun get-relative (x y direction)
  (let
    ((*x* x)
     (*y* y))
    (funcall direction)
    (list *x* *y*)))

#|

(get-relative 0 0 'nw)

|#

(defun get-adjacent (point)
  (mapcar (lambda (dir) (get-relative (first point) (second point) dir))
          '(nw ne e se sw w)))
#|
 
(get-adjacent (list 0 0))

|#


(defun make-candidates (blacks)
  (remove-duplicates
    (append
      blacks
      (mapcan #'get-adjacent blacks))
    :test #'equalp))

(defun one-iteration (blacks)
  (labels
    ((blackp (point)
       (when (member point blacks :test #'equalp)
          t))
     (black-rule (point)
       (case (count-if #'blackp (get-adjacent point))
         ((1 2) (list point))
         (otherwise nil)))
     (white-rule (point)
       (case (count-if #'blackp (get-adjacent point))
         (2 (list point))
         (otherwise nil))))
    (let
      ((candidates (make-candidates blacks)))
      (mapcan 
        (lambda (p)
          (if (blackp p)
            (black-rule p)
            (white-rule p)))
        candidates))))


(length (one-iteration *initial-blacks*))


(defun iterate (initial f n)
  (if (= n 0)
    initial
    (let*
      ((i (copy-tree initial))
       (new (funcall f i)))
      (iterate new f (- n 1)))))


(length (iterate *initial-blacks* #'one-iteration 100))
