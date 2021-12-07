
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

(length (remove-if
         #'evenp
         (occurrences
           (mapcar #'eval-line *parsed*))
         :key #'cdr))
