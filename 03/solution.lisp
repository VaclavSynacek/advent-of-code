
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


(setf *test-data*
  (list "00100"
        "11110"
        "10110"
        "10111"
        "10101"
        "01111"
        "00111"
        "11100"
        "10000"
        "11001"
        "00010"
        "01010"))


(defun second-half (vals comp i)
  (let* ((current-digits (mapcar
                           (lambda (rec)
                             (elt rec i))
                           vals))
         (winner (if (funcall comp
                       (count #\1 current-digits)
                       (count #\0 current-digits))
                     #\1
                     #\0))
         (new-vals (remove-if-not
                     (lambda (rec)
                       (eql winner
                            (elt rec i)))
                     vals)))
    (if (= 1 (length new-vals))
      (first new-vals)
      (second-half new-vals comp (+ i 1)))))


(second-half *test-data* #'>= 0)
(second-half *test-data* #'< 0)


(setf *oxygen*
      (parse-integer
       (second-half *input-numbers* #'>= 0)
       :radix 2))

(setf *co2*
      (parse-integer
       (second-half *input-numbers* #'< 0)
       :radix 2))

(* *oxygen* *co2*)



