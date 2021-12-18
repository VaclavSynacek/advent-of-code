(ql:quickload '(:alexandria :arrows :cl-zipper))

(use-package :arrows)

(defvar *ex1* "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")


(defun lispify (s)
  (-<>> s
        (substitute #\( #\[)
        (substitute #\) #\])
        (substitute #\Space #\,)
        (read-from-string)))


(defvar *loc*)

(setf *loc*
  (cl-zipper:make-zipper (lispify *ex1*)))


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
      (values loc nil)
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


(maybe-explode (cl-zipper:make-zipper (lispify "[[[[[9,8],1],2],3],4]")))

(maybe-explode (cl-zipper:make-zipper (lispify "[7,[6,[5,[4,[3,2]]]]]")))

(maybe-explode (cl-zipper:make-zipper (lispify "[[6,[5,[4,[3,2]]]],1]")))

(maybe-explode (cl-zipper:make-zipper (lispify "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")))

(maybe-explode (cl-zipper:make-zipper (lispify "")))


(find-keyword *problem*)

(->> (maybe-explode *problem*)
  (cl-zipper:node)
  (cl-zipper:make-zipper)
  (find-keyword))

*problem*

(maybe-explode *loc*)

(defvar *problem*)

(setf *problem*
  (->> *loc*
    (maybe-explode)
    (cl-zipper:node)
    (cl-zipper:make-zipper)))

(maybe-explode *problem*)

(->> *loc*
  (maybe-explode)
  (maybe-explode))

(find-explode (cl-zipper:make-zipper '(1 (2 3))))

(-<> (find-explode *loc*)
  (cl-zipper:replace <> :zero)
  (cl-zipper:root))

(-<> (find-explode *loc*)
  (cl-zipper:replace <> :zero)
  (cl-zipper:up)
  (cl-zipper:up)
  (cl-zipper:up)
  (cl-zipper:up)
  (find-keyword))

(-<> *loc*
  (cl-zipper:down)
  (cl-zipper:down)
  (cl-zipper:right)
  (cl-zipper:down)
  (cl-zipper:right)
  (cl-zipper:down)
  (cl-zipper:right)
  (should-explode))

(-<> *loc*
  (cl-zipper:down)
  (cl-zipper:down)
  (cl-zipper:down)
  (four-deep))

(-<> *loc*
  (cl-zipper:up))
