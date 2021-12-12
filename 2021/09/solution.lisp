

(defun read-line-of-numbers (s)
  (loop for ch in (coerce s 'list)
        collect (parse-integer (string ch))))

(defvar *all*)

(with-open-file (stream "input.txt")
  (setf *all*
    (loop for line = (read-line stream nil)
          while line
          collect
            (read-line-of-numbers line))))

(defvar *size*)

(setf *size* (length *all*))

(defun x (point)
  (first point))

(defun y (point)
  (second point))

(defun inside-p (p)
  (and
    (< -1 (x p) *size*)
    (< -1 (y p) *size*)))

(defun nexts (p)
  (remove-if-not
    #'inside-p
    (list
      (list (x p) (1+ (y p)))
      (list (x p) (1- (y p)))
      (list (1+ (x p)) (y p))
      (list (1- (x p)) (y p)))))

(defun get-value (p)
  (elt (elt *all* (y p)) (x p)))

(defun lowest-p (p)
  (< (get-value p)
     (apply #'min (mapcar #'get-value (nexts p)))))

(defvar *lowest*)

(setf *lowest*
  (remove nil
    (loop for x from 0 to (1- *size*)
          append (loop for y from 0 to (1- *size*)
                       collect (if (lowest-p (list x y))
                                 (list x y))))))

(apply #'+ (mapcar
            (lambda (p)
              (1+ (get-value p)))
            *lowest*))

;------------------------

(defun in-basin-p (p)
  (< (get-value p) 9))
    
(defun next-in-basin (p)
  (remove-if-not
    #'in-basin-p
    (nexts p)))

(defun with-next-in-basin (l)
  (remove-duplicates
    (append
      l
      (mapcan #'next-in-basin l))
    :test #'equalp))


(defun whole-basin (l)
  (let
    ((with (with-next-in-basin l)))
    (if (= (length l)
           (length with))
       l
       (whole-basin with))))

(defun set-equal (a b)
  (= 0
     (length
      (set-difference a b :test #'equalp))))

(apply
  #'*
  (subseq
    (sort
      (mapcar #'length
             (remove-duplicates
              (mapcar
                (lambda (p)
                  (whole-basin (list p)))
                *lowest*)
              :test #'set-equal))
      #'>)
    0 3))
