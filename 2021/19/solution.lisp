(declaim (optimize (speed 3) (safety 0) (debug 0)))

(ql:quickload '(:alexandria :arrows :cl-zipper :str))

(use-package :arrows)

(defstruct point
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (z 0 :type fixnum))

(defun make-p (x y z)
  (declare (type fixnum x y z))
  (make-point :x x :y y :z z))


(defun spacify (s ch)
  (substitute
    #\space
    ch
    s))

(defun read-line-of-numbers (s)
  (read-from-string
    (concatenate 'string
                  "(" s ")")))


(defun read-scanner (strm)
  (read-line strm)
  (loop for line = (read-line strm nil)
            while (not (string= line ""))
            collect
              (-<>> line
                    (spacify <> #\,)
                    (read-line-of-numbers)
                    (apply #'make-p))))


(defun read-scanners (strm)
  (loop for scanner = (ignore-errors (read-scanner strm))
        while scanner
        collect scanner))


(defun read-input-file (file)
  (with-open-file (strm file)
    (read-scanners strm)))


(defun p= (p1 p2)
  (declare (type point p1 p2))
  (and
    (= (point-x p1) (point-x p2))
    (= (point-y p1) (point-y p2))
    (= (point-z p1) (point-z p2))))

(defun p+ (p1 p2)
  (declare (type point p1 p2))
  (make-p
    (+ (point-x p1) (point-x p2))
    (+ (point-y p1) (point-y p2))
    (+ (point-z p1) (point-z p2))))

(defun p- (p1 p2)
  (declare (type point p1 p2))
  (make-p
    (- (point-x p1) (point-x p2))
    (- (point-y p1) (point-y p2))
    (- (point-z p1) (point-z p2))))

(defun p* (p1 p2)
  (declare (type point p1 p2))
  (make-p
    (* (point-x p1) (point-x p2))
    (* (point-y p1) (point-y p2))
    (* (point-z p1) (point-z p2))))


(defvar -a-rotations-)

(setf -a-rotations-
  (list
    (list  1  1  1)
    (list  1  1 -1)
    (list  1 -1  1)
    (list  1 -1 -1)
    (list -1  1  1)
    (list -1  1 -1)
    (list -1 -1  1)
    (list -1 -1 -1)))


(defvar -b-rotations-) 

(setf -b-rotations- nil)

#|

(setf -b-rotations- 
  (list
    (list  'x  'y  'z)
    (list  'y  'z 'x)
    (list  'z 'x  'y)))

|#

(alexandria:map-permutations
  (lambda (p)
    (push p -b-rotations-))
  (list 'x 'y 'z))

-b-rotations-


(defun rot-b-fn (rot)
  (lambda (p)
    (make-p
      (slot-value p (first rot))
      (slot-value p (second rot))
      (slot-value p (third rot)))))

(defun rot-a-fn (rot)
  (lambda (p)
    (p* p (apply #'make-p rot))))

(defvar *all-rotating-fns*)

(setf *all-rotating-fns*
  (loop for a in -a-rotations-
    append (loop for b in -b-rotations-
              collect (let
                        ((a a)
                         (b b))
                        (lambda (p)
                         (format nil "lambda with ~a and ~a ~%" a b)
                         (funcall (rot-b-fn b)
                           (funcall (rot-a-fn a) p)))))))

(loop for fn in *all-rotating-fns*
  collect (funcall fn (make-p 4 5 6)))

(defun translate-fn (difference-p)
  (lambda (p)
    (p+ p difference-p)))

(defun find-difference (scanner-a scanner-b)
  (loop for b in scanner-b
    do (loop for a in scanner-a
          do (let*
                ((diff (p- a b))
                 (diff-fn (translate-fn diff))
                 (b-translated (mapcar diff-fn scanner-b))
                 (inter (intersection scanner-a b-translated :test #'p=)))
                (if (>= (length inter) 12)
                  (return-from find-difference b-translated))))))
                  ;(format t "not overlaping with diff ~a only ~a ~%" diff (length inter)))))))


(defun find-difference-with-rotations (scanner-a scanner-b)
  (loop for fn in *all-rotating-fns*
        for i = 0 then (1+ i)
        for rotated-b = (mapcar fn scanner-b)
        for rotated-and-translated-b = (find-difference scanner-a rotated-b)
        do (if rotated-and-translated-b
             (return-from
               find-difference-with-rotations
               rotated-and-translated-b)))) 
             ;(format t "unsuccessfull rotation ~a ~%" i))))

#|

(find-difference-with-rotations
  (list
    (make-p 0 0 0)
    (make-p 1 1 1)
    (make-p 2 2 2)
    (make-p 2 2 3)
    (make-p 2 3 3))
  (list
    (make-p 3 -3 -3)
    (make-p 3 -3 -4)
    (make-p 3 -4 -4)
    (make-p 9 -9 -9)))

|#

(defun maybe-merge (a b)
  (format t "maybe-merge called with a ~a long, b ~a long~%"
          (length a)
          (length b))
  (let
    ((difference (find-difference-with-rotations a b)))
    (if difference
      (remove-duplicates
        (append a difference)
        :test #'p=))))

(defun merge-all-into (acc scanners)
  (format t "merge-all called with acc ~a long, scanners ~a long~%"
          (length acc)
          (length scanners))
  (when (zerop (length scanners))
    (return-from merge-all-into acc))
  (loop for s in scanners
        for i = 1 then (1+ i)
        do (let*
             ((merge (maybe-merge acc s)))
             (when merge
               (format t "after ~a tries, we have merge with  total ~a~%"
                       i
                       (length merge))
               (return-from
                 merge-all-into
                 (merge-all-into merge (remove s scanners :test #'equalp))))))
  nil)


(defvar *result*)

(setf *result*
  (merge-all-into
    (first (read-input-file "input.txt"))
    (rest (read-input-file "input.txt"))))
    
(length *result*)
