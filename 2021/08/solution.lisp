(ql:quickload :split-sequence)


(defun spacifie (s ch)
  (substitute
    #\space
    ch
    s))

(defun read-line-of-input (s)
  (remove ""
    (split-sequence:split-sequence #\space s)
    :test #'string-equal))


(read-from-string
  (concatenate 'string
                "(" s ")"))

(with-open-file (stream "input.txt")
  (setf *all-lines*
    (loop for line = (read-line stream nil)
          while line
          collect
            (read-line-of-input
              (spacifie
                line
                #\|)))))

(defun output (line)
  (subseq line 10 14))

(length (remove-if-not
         (lambda (l)
           (member (length l) '(2 3 4 7)))
         (mapcan #'output *all-lines*)))

;-----------------------------------------

(defvar *digits*)

(setf *digits*
      '((0 "abcefg")
        (1 "cf")
        (2 "acdeg")
        (3 "acdfg")
        (4 "bcdf")
        (5 "abdfg")
        (6 "abdefg")
        (7 "acf")
        (8 "abcdefg")
        (9 "abcdfg")))

; from https://stackoverflow.com/questions/2087693/how-can-i-get-all-possible-permutations-of-a-list-with-common-lisp

(defun all-permutations (list)
  (cond
    ((null list) nil)
    ((null (cdr list)) (list list))
    (t (loop for element in list
         append (mapcar (lambda (l) (cons element l))
                        (all-permutations (remove element list)))))))


(defun s2l (str)
  (coerce str 'list))

(defun l2s (str)
  (coerce str 'string))

(defvar *permutations*)

(setf *permutations*
      (all-permutations (s2l "abcdefg")))

(first (all-permutations (s2l "abcdefg")))

(defun get-translate-fn (permutation)
  (let
    ((mapping (mapcar #'cons (s2l "abcdefg") permutation)))
    (lambda (ch)
      (cdr (find ch mapping :key #'car)))))


(defvar *translate-functions*)

(setf *translate-functions*
      (mapcar #'get-translate-fn *permutations*))


(defun wires-to-digit (wires)
  (car (find (l2s (sort wires #'char-lessp))
             *digits*
             :key #'second
             :test #'string-equal)))


(defun get-string-translate-fn (trf)
  (lambda (s)
    (mapcar (lambda (ch)
              (funcall trf ch))
            (s2l s))))

(setf *tf*
      (get-string-translate-fn (second *translate-functions*)))
      

(funcall *tf* "efg")

(wires-to-digit (s2l "fc"))

(defvar *translate-functions*)

(setf *translate-functions*
      (mapcar
        #'get-string-translate-fn
        (mapcar
          #'get-translate-fn
          *permutations*)))

(funcall
  (second *translate-functions*)
  "agc")

(defun decode-line (l)
  (loop for fn in *translate-functions*
        do (let
             ((translated (mapcar
                            (lambda (s)
                              (wires-to-digit
                                (funcall fn s)))
                            l)))
             (when (notany #'null translated)
               (return-from decode-line translated)))))

(defun digits-to-number (l)
  (+ 
     (* (first l) 1000)
     (* (second l) 100)
     (* (third l) 10)
     (* (fourth l) 1)))


(apply #'+ (mapcar
            (lambda (l)
              (digits-to-number (output (decode-line l))))
            *all-lines*))


