
(defvar *input-numbers*)

(setf *input-numbers*  
  (with-open-file (stream "input.txt")
    (loop for line = (read-line stream nil)
          while line
          collect (read-from-string
                    (concatenate 'string "(" line ")")))))


(defvar x)
(defvar y)

(setf x 0)
(setf y 0)

(defun forward (i)
  (incf x i))
(defun down (i)
  (incf y i))
(defun up (i)
  (decf y i))

(mapc #'eval *input-numbers*)
(* x y)


(setf x 0)
(setf y 0)
(defvar aim)
(setf aim 0)


(defun forward (i)
  (incf x i)
  (incf y (* i aim)))
(defun down (i)
  (incf aim i))
(defun up (i)
  (decf aim i))

(mapc #'eval *input-numbers*)
(* x y)

#|
(defun process (code)
  (let
    ((x 0)
     (y 0))
    (flet
      ((forward (i)
         (incf x i))
       (down (i)
         (decf y i))
       (up (i)
         (incf y i)))
      (mapc #'eval code)
      (list x y))))

(process *input-numbers*)
|#
