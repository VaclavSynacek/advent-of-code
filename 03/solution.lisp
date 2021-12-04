
(defvar *input-numbers*)

(setf *input-numbers*  
  (with-open-file (stream "input.txt")
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(setf *binary*
  (concatenate
    'string
    (loop for i from 0 to (1- (length (first *input-numbers*)))
         for s = (loop for num in *input-numbers*
                       when (eql #\1 (elt num i))
                       sum 1)
         collect (if (> s (/ (length *input-numbers*) 2))
                   #\1 
                   #\0))))

(setf *gamma*
  (parse-integer *binary* :radix 2))

(setf *epsilon*
  (logxor *gamma* #b111111111111))

(format nil "~b" *gamma*)
(format nil "~b" *epsilon*)

(* *gamma* *epsilon*)


