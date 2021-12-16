

(defun read-line-of-numbers (s)
  (loop for ch in (coerce s 'list)
        collect (parse-integer (string ch))))

(defvar *start*)
(defvar *subs*)

(with-open-file (stream "input.txt")
  (setf *start*
        (coerce (read-line stream nil) 'list))
  (read-line stream nil)      
  (setf *subs*
        (loop for line = (coerce (read-line stream nil) 'list)
              while line
              collect
                (cons (cons (first line)
                           (second line))
                      (seventh line)))))


(defun interpolate (l subs)
  (let
    ((new (list (car l))))
    (loop for i from 0 to (- (length l) 2)
      do (let*
            ((left (elt l i))
             (right (elt l (1+ i)))
             (inter (cdr (find
                           (cons left right)
                           subs
                           :key #'car
                           :test #'equalp))))
            (setf new
                  (cons right 
                        (cons inter
                              new)))))
    (reverse new)))


(defun interpolate-n-times (start subs n)
  (format t "startind interpolate for n=~a~%" n)
  (if (= n 1)
    (interpolate start subs)
    (interpolate-n-times (interpolate start subs) subs (1- n))))


(defun frequencies (l)
  (let
    ((uniq (remove-duplicates l)))
    (mapcar
      (lambda (i)
        (list i (count i l)))
      uniq)))

(defun score (freqs)
  (- (reduce #'max (mapcar #'second freqs))
     (reduce #'min (mapcar #'second freqs))))

(score (frequencies 
        (interpolate-n-times *start* *subs* 10)))

;---------------------------

;(score (frequencies 
;         (interpolate-n-times *start* *subs* 40))))


(defun convert-start-to-pairs (start) 
  (let
    ((hash (make-hash-table :test #'equalp)))
    (loop for i from 0 to (- (length start) 2)
      do (incf (gethash 
                 (cons
                   (elt start i)
                   (elt start (1+ i)))
                 hash
                 0)))
    hash))

(defun convert-start-to-chars (start) 
  (let
    ((hash (make-hash-table :test #'equalp)))
    (mapc
      (lambda (ch)
        (incf (gethash ch hash 0)))
      start)
    hash))


(defun interpolate (hashtables subs)
  (let
    ((old-pairs (first hashtables))
     (new-pairs (make-hash-table :test #'equalp))
     (chars (second hashtables)))
    (loop for k being the hash-keys in old-pairs using (hash-value v)
      do (let*
            ((left (car k))
             (right (cdr k))
             (inter (cdr (find
                           (cons left right)
                           subs
                           :key #'car
                           :test #'equalp))))
            (incf
              (gethash (cons left inter) new-pairs 0)
              v)
            (incf
              (gethash (cons inter right) new-pairs 0)
              v)
            (incf
              (gethash inter chars 0)
              v)))
    (list new-pairs chars)))


(defvar *result*)

(setf *result*
  (interpolate-n-times (list
                         (convert-start-to-pairs *start*)
                         (convert-start-to-chars *start*))
                       *subs*
                      40))


(defun score2 (counts)
  (- (reduce #'max counts)
     (reduce #'min counts)))

(score2
  (loop for k being the hash-keys in (second *result*) using (hash-value v)
   collect v))

