
(defun spacifie (s ch)
  (substitute
    #\space
    ch
    s))

(defun read-line-of-numbers (s)
  (read-from-string
    (concatenate 'string
                  "(" s ")")))


(defvar *small-input*
  (read-line-of-numbers
    (spacifie
      "3,4,3,1,2"
      #\,)))

(defvar *big-input*
  (read-line-of-numbers
    (spacifie
      "4,1,1,4,1,1,1,1,1,1,1,1,3,4,1,1,1,3,1,3,1,1,1,1,1,1,1,1,1,3,1,3,1,1,1,5,1,2,1,1,5,3,4,2,1,1,4,1,1,5,1,1,5,5,1,1,5,2,1,4,1,2,1,4,5,4,1,1,1,1,3,1,1,1,4,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,1,1,2,1,1,1,1,1,1,1,2,4,4,1,1,3,1,3,2,4,3,1,1,1,1,1,2,1,1,1,1,2,5,1,1,1,1,2,1,1,1,1,1,1,1,2,1,1,4,1,5,1,3,1,1,1,1,1,5,1,1,1,3,1,2,1,2,1,3,4,5,1,1,1,1,1,1,5,1,1,1,1,1,1,1,1,3,1,1,3,1,1,4,1,1,1,1,1,2,1,1,1,1,3,2,1,1,1,4,2,1,1,1,4,1,1,2,3,1,4,1,5,1,1,1,2,1,5,3,3,3,1,5,3,1,1,1,1,1,1,1,1,4,5,3,1,1,5,1,1,1,4,1,1,5,1,2,3,4,2,1,5,2,1,2,5,1,1,1,1,4,1,2,1,1,1,2,5,1,1,5,1,1,1,3,2,4,1,3,1,1,2,1,5,1,3,4,4,2,2,1,1,1,1,5,1,5,2"
      #\,)))


(defun pass-time (fish)
  (loop for f in fish
        append (cond
                 ((> f 0) (list (1- f)))
                 ((= 0 f) (list 6 8)))))

(defun n-times (input n)
  (let
    ((result input)
     (counter 0))
    (loop repeat n
          do (progn
               (setf result (pass-time result))
               (incf counter)))
               ;(format t "~a iterations~%" counter)))
    (length result)))


(n-times *small-input* 18)

(n-times *small-input* 80)

(n-times *big-input* 80)

;(n-times *big-input* 256)
; Ooops, this kills lisp and probably everything else

;--------------------------------

(defun compact (fish)
  (loop for i from 0 to 8
        collect (count i fish)))

;(compact *small-input*)
          
(defun pass-time (fish)
  (let*
    ((breeding (* 1 (car fish)))
     (new-fish (append (cdr fish) (list 0))))
    ;(format t "pass-------start~%")
    ;(format t "fish: ~a~%" fish)
    ;(format t "new-fish: ~a~%" new-fish)
    ;(format t "-breeding: ~a~%" breeding)
    (incf (elt new-fish 6) breeding)
    ;(format t "new-fish: ~a~%" new-fish)
    (incf (elt new-fish 8) breeding)
    ;(format t "new-fish: ~a~%" new-fish)
    (copy-list new-fish)))

(defun n-times (input n)
  (let
    ((result input)
     (counter 0))
    (loop repeat n
          do (progn
               (setf result (pass-time result))
               (incf counter)))
               ;(format t "at ~a iterations ~a fish: ~a ~%"
               ;       counter
               ;       (apply #'+ result)
               ;       result)))
    (apply #'+ result)))


(n-times (compact *small-input*) 18)

(n-times (compact *small-input*) 80)

(n-times (compact *big-input*) 80)

(n-times (compact *big-input*) 256)

