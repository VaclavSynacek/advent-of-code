(ql:quickload '(:alexandria :arrows :cl-zipper :str))

(use-package :arrows)

(defvar *ex1* "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")


(defun lispify (s)
  (-<>> s
        (substitute #\( #\[)
        (substitute #\) #\])
        (substitute #\Space #\,)
        (read-from-string)))


(defun depth (init)
  (loop for i = 1 then (1+ i)
        for loc = init then (cl-zipper:up loc)
        while (cl-zipper:up loc)
        finally (return i)))

(defun four-deep (loc)
  (> (depth loc) 4))

(defun simple-number (loc)
  (let
    ((here (cl-zipper:node loc)))
    (and
      (listp here)
      (numberp (first here))
      (numberp (second here)))))

(defun should-explode (loc)
  (and
    (four-deep loc)
    (simple-number loc)))

(defun find-explode (loc)
  (loop for l = loc then (cl-zipper:next l)
        do (if (should-explode l)
             (return l))
        while (cl-zipper:next l)))

(defun find-root (loc)
  (loop for l = loc then (cl-zipper:up l)
        while (cl-zipper:up l)
        finally (return l)))

(defun find-keyword (loc)
  (loop for l = (find-root loc) then (cl-zipper:next l)
        do (if (keywordp (cl-zipper:node l))
             (return l))
        while (cl-zipper:next l)))

(defun find-nearest-left (loc)
  (loop for l = loc then (cl-zipper:prev l)
          do (if (numberp (cl-zipper:node l))
               (return l))
          while (cl-zipper:prev l)))

(defun find-nearest-right (loc)
  (loop for l = loc then (cl-zipper:next l)
          do (if (numberp (cl-zipper:node l))
               (return l))
          while (cl-zipper:next l)))

(defun maybe-explode (loc)
  (let
    ((explode-here (find-explode loc)))
    (if (not explode-here)
      (values (find-root loc) nil)
      (let*
        ((left  (first (cl-zipper:node explode-here)))
         (right (second (cl-zipper:node explode-here)))
         (zero  (-<> explode-here
                  (cl-zipper:replace <> :zero)))
         (nearest-left (find-nearest-left (find-keyword zero)))
         (after-left (if (not nearest-left)
                       zero
                       (-<> nearest-left
                            (cl-zipper:edit <> #'+ left))))
         (nearest-right (find-nearest-right (find-keyword after-left)))
         (after-right (if (not nearest-right)
                        after-left
                        (-<> nearest-right
                             (cl-zipper:edit <> #'+ right))))   
         (after-all (-<> after-right
                         (find-keyword)
                         (cl-zipper:replace <> 0))))
        (values
          (cl-zipper:make-zipper (cl-zipper:root after-all))
          t)))))

(defun find-split (loc)
  (loop for l = (find-root loc) then (cl-zipper:next l)
        do (if (and (numberp (cl-zipper:node l))
                    (>= (cl-zipper:node l) 10))
             (return l))
        while (cl-zipper:next l)))

(defun split-num (n)
  (list
    (floor n 2)
    (ceiling n 2)))

(defun maybe-split (loc)
  (let
    ((split-here (find-split loc)))
    (if (not split-here)
      (values (find-root loc) nil)
      (let*
        ((val (split-num (cl-zipper:node split-here)))
         (after-split (cl-zipper:replace split-here val)))
        (values
          (find-root after-split)
          t)))))


(defun maybe-explode-or-split (loc)
  (multiple-value-bind (result done?)
      (maybe-explode loc)
      (if done?
        (values result done?)
        (maybe-split loc))))

(maybe-explode-or-split *loc*)

(defun reducee (loc)
  ;(format t "reducind ~a ..." (cl-zipper:node loc))
  (multiple-value-bind (result done?)
    (maybe-explode-or-split loc)
    ;(format t "was ~a ~%" done?)
    (if done?
      (reducee result)
      loc)))

(defun reduce-list (l)
  (->> l
       (cl-zipper:make-zipper)
       (reducee)
       (cl-zipper:node)))

(defun plus (a b)
  (->> (list a b)
       (reduce-list)))

(defun magnitude (lst)
  (let*
    ((left (first lst))
     (right (second lst))
     (left-result (* 3 (if (numberp left)
                         left
                         (magnitude left))))
     (right-result (* 2 (if (numberp right)
                          right
                          (magnitude right)))))
    (+ left-result right-result)))

(-<>> "input.txt"
  (alexandria:read-file-into-string)
  (str:lines)
  (mapcar #'lispify)
  (reduce #'plus)
  (magnitude))

;-------------------------------------

(defun pairs (all)
  (->>
    (loop for i in all
      append (loop for j in all
                collect (list i j)))
    (remove-if
      (lambda (c)
        (equalp (first c) (second c))))))


(-<>> "input.txt"
  (alexandria:read-file-into-string)
  (str:lines)
  (mapcar #'lispify)
  (pairs)
  (mapcar (lambda (c) (magnitude (plus (first c) (second c)))))
  (apply #'max))
