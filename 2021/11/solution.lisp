

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
      (list (1- (x p)) (y p))
      (list (1+ (x p)) (1+ (y p)))
      (list (1+ (x p)) (1- (y p)))
      (list (1- (x p)) (1+ (y p)))
      (list (1- (x p)) (1- (y p))))))

(defmacro value (p)
  `(elt (elt *all* (y ,p)) (x ,p)))

(defun energize-all ()
  (loop for y to (1- *size*)
    do (loop for x to (1- *size*)
        do (incf (value (list x y))))))

(defun get-first-energized ()
  (loop for y to (1- *size*)
    do (loop for x to (1- *size*)
        do (when (value (list x y))
             (when (> (value (list x y)) 9)
                (return-from
                  get-first-energized
                  (list x y)))))))

(defvar *counter*)

(setf *counter* 0)

(defun light-up (p)
  (setf (value p) nil)
  (incf *counter*)
  (mapc (lambda (p)
          (when (value p)
            (incf (value p))))
        (nexts p)))

(defun light-up-all ()
  (loop while (get-first-energized)
    do (light-up (get-first-energized))))

(defun reset-to-all ()
  (loop for y to (1- *size*)
    do (loop for x to (1- *size*)
        do (unless (value (list x y))
              (setf (value (list x y)) 0)))))


(defun iterate-once ()
  (energize-all)
  (light-up-all)
  (reset-to-all))


(defun iterate (x)
  (loop repeat x
    do (iterate-once)))

#|

(let ((*print-right-margin* 25))
  (print *all*)
  nil)

|#

(iterate 100)

*counter*

;------------------------


(defun all-flashed-p ()
  (loop for y to (1- *size*)
    do (loop for x to (1- *size*)
        do (when (value (list x y))
             (return-from all-flashed-p nil))))
  t)


(defun iterate-till-sync ()
  (loop for i from 1 to 1000
    do (progn
         (energize-all)
         (light-up-all)
         (when (all-flashed-p)
            (return-from iterate-till-sync i))
         (reset-to-all))))


(iterate-till-sync)
