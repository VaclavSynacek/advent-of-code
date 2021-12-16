(ql:quickload :arrows)

(use-package :arrows)

(defvar *input*)

(with-open-file (stream "input.txt")
  (setf *input*
        (read-line stream nil)))


(defun hex2bin (s)
  (->> (parse-integer s :radix 16)
     (format nil (format nil "~~~a,'0b" (* 4 (length s))))))

(defun bin2dec (s)
  (typecase s
    (string (parse-integer s :radix 2))
    (list (bin2dec (coerce s 'string)))))

(defun read-digits (s n)
  (format t "togeterh readen the following ~a BITS ~%" n)
  (loop repeat n
    collect (read-char s)))

(defun read-packets (s)
  s)

(defun read-packet (s)
  (let*
    ((version (bin2dec (read-digits s 3)))
     (typ (bin2dec (read-digits s 3)))
     (value (if (= 4 typ)
              (bin2dec (loop for next = (read-digits s 1)
                            for n = (read-digits s 4)
                            append n
                            until (equalp next '(#\0))))
              (if (equalp '(#\0) (read-digits s 1))
                (let*
                  ((len (bin2dec (read-digits s 15)))
                   (sub-s (coerce (read-digits s len) 'string)))
                  (format t "doing sub with length ~a~%" len)
                  (read-packets sub-s))
                (loop repeat (bin2dec (read-digits s 11))
                  collect (read-packet s))))))
    (list :version version
          :type typ
          :value value)))
     

(defun read-packets (string)
  (let
    ((s (make-string-input-stream string)))
    (loop for packet = (ignore-errors (read-packet s))
       while packet
       collect packet)))


(defun sum-versions (packets)
 (apply #'+ (mapcar
              (lambda (p)
                (+ (getf p :version)
                   (if (listp (getf p :value))
                     (sum-versions (getf p :value))
                     0)))
              packets)))

#|
 
(sum-versions (read-packets (hex2bin "38006F45291200")))

(sum-versions (read-packets (hex2bin "8A004A801A8002F478")))

(sum-versions (read-packets (hex2bin "620080001611562C8802118E34")))

(sum-versions (read-packets (hex2bin "C0015000016115A2E0802F182340")))

(sum-versions (read-packets (hex2bin "A0016C880162017C3686B18A3D4780")))

|#

(sum-versions (read-packets (hex2bin *input*)))

;------------------------------------------

(defvar *parsed*)

(setf *parsed* (first (read-packets (hex2bin *input*))))


(defun p> (a b)
  (if (> a b)
    1
    0))

(defun p< (a b)
  (if (< a b)
    1
    0))

(defun p= (a b)
  (if (= a b)
    1
    0))

(defun comp (in)
  (if (numberp (getf in :value))
    (getf in :value)
    (cons (case (getf in :type)
            (0 '+)
            (1 '*)
            (2 'min)
            (3 'max)
            (5 'p>)
            (6 'p<)
            (7 'p=))
      (mapcar #'comp (getf in :value)))))

(eval (comp *parsed*))
