(ql:quickload :split-sequence)


(defun read-line-of-input (s)
  (split-sequence:split-sequence #\- s))


(defvar *all*)

(with-open-file (stream "input.txt")
  (setf *all*
    (loop for line = (read-line stream nil)
          while line
          collect
            (read-line-of-input
              line))))

(defun lines-from (lines from)
  (remove-if-not
    (lambda (line)
      (or (string= from (first line))
          (string= from (second line))))
    lines))

(defun lines-not-from (lines from)
  (remove-if
    (lambda (line)
      (or (string= from (first line))
          (string= from (second line))))
    lines))

(defun destinations (lines from)
  (mapcar
    (lambda (line)
      (if (string= (first line) from)
        (second line)
        (first line)))
    (lines-from lines from)))

(defun big-cave (str)
  (upper-case-p (first (coerce str 'list))))

(defun filter-valid-dests (candidates prev-steps)
  (set-difference
    candidates
    (remove-if #'big-cave prev-steps)
    :test #'string=))

(defun get-all-ways (valid-lines prev-steps cur-pos)
  (if (string= "end" cur-pos)
    (list (cons cur-pos prev-steps))
    (let*
      ((lines (copy-tree valid-lines))
       (candidates (destinations lines cur-pos))
       (dests (filter-valid-dests candidates (cons cur-pos prev-steps))))
      (if (null dests)
        (list prev-steps)
        (loop for d in dests
              append (get-all-ways
                       lines
                       (cons cur-pos prev-steps)
                       d))))))

(defun get-all (lines)
  (remove-if-not
    (lambda (way)
      (string= "end" (car (last way))))
    (mapcar #'reverse (get-all-ways lines '() "start"))))

(length (get-all *all*))

;-----------------------------------


(defun filter-valid-dests (candidates prev-steps)
  (let*
    ((visited-small-caves (remove-if #'big-cave prev-steps))
     (already-twice-p (> (length visited-small-caves)
                         (length (remove-duplicates
                                   visited-small-caves
                                   :test #'string=)))))
    (remove "start"
            (set-difference
              candidates
              (if already-twice-p
                (remove-if #'big-cave prev-steps)
                '())
              :test #'string=)
            :test #'string=)))

(length (get-all *all*))
