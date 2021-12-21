(declaim (optimize (speed 3) (safety 0) (debug 0)))

(ql:quickload '(:alexandria :arrows :cl-zipper :str))

(use-package :arrows)

(defun hash2binstring (str)
  (-<>> str
    (substitute #\1 #\#)
    (substitute #\0 #\.)))

(defun read-enhancer (str)
  (-<>> str
    (hash2binstring)
    (coerce <> 'vector)
    (map 'vector (lambda (ch)
                   (if (char= ch #\1) 1 0)))))
    ;(parse-integer <> :radix 2)))

(defun read-image (strm)
  (let
    ((initial (loop for line = (read-line strm nil)
                    while (not (string= line ""))
                    collect (-<>> line
                                  (hash2binstring)
                                  (coerce <> 'list)
                                  (mapcar (lambda (ch)
                                            (if (char= ch #\1) 1 0)))))))
    (make-array
      (list (length initial) (length (first initial))) 
      :initial-contents initial
      :element-type '(integer 2))))

(defun read-input-file (file)
  (with-open-file (strm file)
    (let
      ((enhancer (read-enhancer (read-line strm)))
       (_ (read-line strm))
       (image (read-image strm)))
      (list
        :enhancer enhancer
        :image image
        :border 0))))

(read-input-file "small-input.txt")


(read-input-file "input.txt")


(defun enlarge-array-by-one (old init copy-p)
  (let*
    ((old-dimensions (array-dimensions old))
     (new-dimensions (mapcar
                      (lambda (d)
                        (+ 2 d))
                      old-dimensions))
     (new (make-array
            new-dimensions
            :initial-element init
            :element-type '(integer 2))))
    (when copy-p
      (loop for y from 0 to (1- (first old-dimensions))
        do (loop for x from 0 to (1- (second old-dimensions))
              do (setf
                   (aref new (1+ y) (1+ x))
                   (aref old y x)))))
    new))

(defun nexts (l)
  (let
    ((y (first l))
     (x (second l)))
    (list
      (list (1- y) (1- x))
      (list (1- y) x)
      (list (1- y) (1+ x))
      (list     y  (1- x))
      (list     y  x)
      (list     y  (1+ x))
      (list (1+ y) (1- x))
      (list (1+ y) x)
      (list (1+ y) (1+ x)))))

(defun shift-up (l)
  (mapcar #'1+ l))

(defun shift-down (l)
  (mapcar #'1- l))

(defun get-value-at (image border p)
  (handler-case
    (apply #'aref image p)
    (sb-int:invalid-array-index-error (e)
      (declare (ignore e))
      border)))

(defun bits2dec (bits)
  (loop for b in (reverse bits)
        for i = 1 then (* i 2)
        sum (* b i) into s
        finally (return s))) 

(defun get-square-under (image border p)
  (-<>> p
    (nexts)
    (mapcar (lambda (n)
              (get-value-at image border n)))))

(defun enhance (enhancer n)
  (the fixnum (svref enhancer n)))

(defun iterate-once (inputs)
  (let*
    ((i-image (getf inputs :image))
     (i-border (getf inputs :border))
     (enhancer (getf inputs :enhancer))
     (n-image (enlarge-array-by-one i-image nil nil))
     (n-dimensions (array-dimensions n-image))
     (n-border (->>
                (loop repeat 9
                      collect i-border)
                (bits2dec)
                (enhance enhancer))))
    (loop for y from 0 to (1- (first n-dimensions))
            do (loop for x from 0 to (1- (second n-dimensions))
                  do (setf
                       (aref n-image y x)
                       (-<>>
                         (list (1- y) (1- x))
                         (get-square-under i-image i-border)
                         (bits2dec)
                         (enhance enhancer)))))
    (list
      :enhancer enhancer
      :image n-image
      :border n-border
      :dimensions n-dimensions)))

(defun count-them (inputs)
  (let*
    ((image (getf inputs :image))
     (dimensions (array-dimensions image))
     (sum 0))
    (loop for y from 0 to (1- (first dimensions))
            do (loop for x from 0 to (1- (second dimensions))
                  do (when (= 1 (aref image y x))
                        (incf sum))))
    sum))



(let*
  ((inputs (read-input-file "input.txt")))
  (->> inputs
    (iterate-once)
    (iterate-once)
    (count-them)))

;-------------------------------------

(count-them
  (loop repeat 51
        for data = (read-input-file "input.txt")
                   then (iterate-once data)
        finally (return data)))
