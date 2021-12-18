
(defvar *test* '((20 30) (-10 -5)))
(defvar *input* '((138 184) (-125 -71)))

(defun vector-add (v1 v2)
  (list
    (+ (first v1) (first v2))
    (+ (second v1) (second v2))))

(vector-add '(2 3) '(4 5))

(defun vector-age (v1)
  (list
    (cond
      ((= 0 (first v1)) 0)
      ((< (first v1) 0) (1+ (first v1)))
      ((> (first v1) 0) (1- (first v1))))
    (1- (second v1))))

(vector-age '(-5 5))

(defun in-target (v target)
  (let
    ((xlim (first target))
     (ylim (second target)))
    (and
      (<= (first xlim) (first v) (second xlim))
      (<= (first ylim) (second v) (second ylim)))))

(in-target '(150 -100) *input*)

(defun lost (v target)
  (let
    ((xlim (first target))
     (ylim (second target)))
    (or
      (> (first v) (second xlim))
      (< (second v) (first ylim)))))

(lost '(185 -125) *input*)

(defun shoot (vect target)
  (loop for pos = '(0 0) then (vector-add pos vec)
        for vec = vect then (vector-age vec) 
        maximize (second pos) into top
        do (if (lost pos target)
             (return-from shoot nil)
             (if (in-target pos target)
               (return-from shoot (values t top))))))

(defun deb (vect target)
  (loop repeat 10
        for pos = '(0 0) then (vector-add pos vec)
        for vec = vect then (vector-age vec) 
        collect pos))

(defun find-all (target)
  (let
    ((maxx (second (first target)))
     (miny -2000)
     (maxy 2000))
    (loop for x from 0 to maxx
          append (loop for y from miny to maxy
                       append (let
                                ((vec (list x y)))
                                (multiple-value-bind (res m)
                                    (shoot vec target)
                                    (if res
                                      (list (list vec m))
                                      nil)))))))


(defun highest (results)
  (find
   (apply #'max (mapcar #'second results))
   results
   :key #'second))

(highest (find-all *input*))

;------------------------------

(length (find-all *input*))

