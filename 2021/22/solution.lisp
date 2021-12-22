(declaim (optimize (speed 3) (safety 0) (debug 0)))

(ql:quickload '(:alexandria :arrows :cl-zipper :str))

(use-package :arrows)

(defun spacify (s ch)
  (substitute
    #\space
    ch
    s))

(defun read-line-of-numbers (s)
  (read-from-string
    (concatenate 'string
                  "(" s ")")))


(defun read-lines (strm)
  (loop for line = (read-line strm nil)
            while line
            collect
              (-<>> line
                    (spacify <> #\=)
                    (spacify <> #\.)
                    (spacify <> #\,)
                    (spacify <> #\x)
                    (spacify <> #\y)
                    (spacify <> #\z)
                    (read-line-of-numbers))))


(defun read-input-file (file)
  (with-open-file (strm file)
    (read-lines strm)))

(defun filter-a (l)
  (remove-if-not
    (lambda (line)
      (and
        (< -50 (second line) 50)
        (< -50 (third line) 50)
        (< -50 (fourth line) 50)))
    l))

(defun compile-l (line)
  `(when (and
           (<= ,(second line) x ,(third line))
           (<= ,(fourth line) y ,(fifth line))
           (<= ,(sixth line) z ,(seventh line)))
     (return-from classify (the fixnum ,(if (eq 'on (first line)) 1 0)))))

(defun compile-tl (l)
  `(defun classify (x y z)
     (declare (optimize (speed 3) (safety 0) (debug 0)))
     (declare (type fixnum x y z))
     ,@(mapcar #'compile-l (reverse l))
     0))

(defun redefine-classify (l)
  (eval (compile-tl l)))
  
(declaim (inline classify))
(->>
  (read-input-file "input.txt")
  (filter-a)
  (redefine-classify))


(defun maxi (l)
  (->> l
    (mapcar
     (lambda(l)
       (remove (first l) l)))
    (apply #'append)
    (mapcar #'abs)
    (apply #'max)))

(->>
  (read-input-file "input.txt")
  (maxi))

(defun calculate ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let
    ((limit 50)
     (result 0))
    (loop for x from (* -1 limit) to limit
          do (progn
               (format t "x=~a~%" x)
               (loop for y from (* -1 limit) to limit
                   do (progn
                        (when (zerop (rem y 1000))
                           (format t "y=~a~%" y))
                        (loop for z from (* -1 limit) to limit
                            do (progn
                                 (incf
                                   (the fixnum result)
                                   (the fixnum (classify x y z)))))))))
    result))

(calculate)

