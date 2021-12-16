(defun make-bundle (risk &optional total best)
  (list
    risk
    (if total
      total
      1000000)
    best))

(defmacro risk (b)
  `(first ,b))
(defmacro total (b)
  `(second ,b))
(defmacro best (b)
  `(third ,b))

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

(setf (total (bundle-at (list 0 0))) 0)

*all*

(defvar *modified*)

(setf *modified* nil)

(defun optimize-one (p)
  (loop for n in (nexts p)
    do (progn
         ;(format t "optimizing ~a with ~a~%" p n)
         ;(format t "cur total ~a new total ~a~%"
         ;          (total (bundle-at p))
         ;          (+ (total (bundle-at n)) (risk (bundle-at p)))
         (when (< (+ (total (bundle-at n)) (risk (bundle-at p)))
                  (total (bundle-at p)))
           ;(format t "yes~%" p n)
           (setf (total (bundle-at p))
                 (+ (total (bundle-at n)) (risk (bundle-at p))))
           (setf (best (bundle-at p))
                 (cons n (copy-tree (best (bundle-at n)))))
           (setf *modified* t)))))

(defun optimize-all ()
  (loop for y to (1- *size*)
    do (loop for x to (1- *size*)
        do (optimize-one (list x y)))))

(defun optimize-all-always (n)
  (format t "optimizing iteration ~a~%" n)
  (setf *modified* nil)
  (optimize-all)
  (when *modified*
    (optimize-all-always (1+ n))))

(optimize-all-always 0)

(total (bundle-at '(99 99)))
