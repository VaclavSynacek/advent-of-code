(defun make-bundle (risk &optional total)
  (list
    risk
    (if total
      total
      1000000)))

(defmacro risk (b)
  `(first ,b))
(defmacro total (b)
  `(second ,b))

(defun read-line-of-numbers (s)
  (loop for ch in (coerce s 'list)
        collect (make-bundle (parse-integer (string ch)))))

(defvar *all*)

(with-open-file (stream "input.txt")
  (setf *all*
    (loop for line = (read-line stream nil)
          while line
          collect
            (read-line-of-numbers line))))

(defvar *size*)

(setf *size* (length *all*))
(assert (= *size* (length (first *all*))))

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

(defmacro bundle-at (p)
  `(elt (elt *all* (y ,p)) (x ,p)))

(defvar *modified*)

(setf *modified* nil)

(defun optimize-one (p)
  (loop for n in (nexts p)
    do (progn
         (when (< (+ (total (bundle-at n)) (risk (bundle-at p)))
                  (total (bundle-at p)))
           (setf (total (bundle-at p))
                 (+ (total (bundle-at n)) (risk (bundle-at p))))
           (incf *modified*)))))

(defun optimize-all ()
  (loop for y to (1- *size*)
    do (loop for x to (1- *size*)
        do (optimize-one (list x y)))))

(defun optimize-all-always (n)
  (format t "optimizing iteration ~a" n)
  (setf *modified* 0)
  (optimize-all)
  (format t " ... optimized ~a points ~%" *modified*)
  (when (> *modified* 0)
    (optimize-all-always (1+ n))))


(defun add-and-wrap (orig inc)
  (if (> (+ orig inc) 9)
    (- (+ orig inc) 9)
    (+ orig inc)))


(defun enlarge-cave (old)
  (let
    ((size (length old)))
    (loop for y from 0 to (1- (* size 5)) 
      collect (loop for x from 0 to (1- (* size 5))
                collect (let*
                          ((copy (copy-tree (bundle-at (list (rem x size) (rem y size)))))
                           (new-risk (add-and-wrap
                                       (risk copy)
                                       (+ (floor x size)
                                          (floor y size)))))
                          (setf (risk copy) new-risk)  
                          copy)))))


(setf *all* 
      (enlarge-cave *all*))

(setf *size* (length *all*))
(assert (= *size* (length (first *all*))))

(defun print-cave ()
  (loop for y from 0 to (1- *size*)
    do (progn
         (loop for x from 0 to (1- *size*)
           do (format t "~a" (risk (bundle-at (list x y)))))
         (format t "~%"))))


; (print-cave)

(setf (total (bundle-at (list 0 0))) 0)

(optimize-all-always 0)

(total (bundle-at '(499 499)))

