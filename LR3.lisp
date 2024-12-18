;Конструктивний підхід
(defun find-min (lst min)
  "Find the minimum value in the list `lst'. `min' is the current smallest value."
  (if (null lst)
      min
      (find-min (cdr lst)
                (if (< (first lst) min)
                    (first lst)
                    min))))

(defun remove-element (lst elem)
  "Remove the first occurrence of `elem' from the list `lst' and return the resulting list."
  (cond
    ((null lst) nil)
    ((eql (first lst) elem) (cdr lst))
    (t (cons (first lst) (remove-element (cdr lst) elem)))))

(defun select1-functional (lst)
  "Sort the list `lst' using the selection sort algorithm(functional)."
  (when lst
    (let ((min (find-min (cdr lst) (first lst))))
      (cons min (select1-functional (remove-element lst min))))))

(defun check-functional (name input expected)
  (format t "~:[FAILED~;PASSED~]... ~a~%"
          (equal (select1-functional input) expected)
          name))

(defun test-select1-functional ()
  (check-functional "test 1.1" '(9 2 5 7 3 0) '(0 2 3 5 7 9))
  (check-functional "test 1.2" '(6 5 4 3 2 1) '(1 2 3 4 5 6))
  (check-functional "test 1.3" '(0 1 2 3 4 5) '(0 1 2 3 4 5))
  (check-functional "test 1.4" '(3 1 2 2 3 1) '(1 1 2 2 3 3))
  (check-functional "test 1.5" '(5) '(5))
  (check-functional "test 1.6" '() '()))



(defun select1-imperative (lst)
  "Sort the list `lst' using the selection sort algorithm(imperative)."
  (let ((sorted-list (copy-list lst))
        (n (length lst)))
    (dotimes (i (1- n))
      (let ((imin i))
        (dotimes (j (- n (1+ i)))
          (let ((icurrent (+ j (1+ i))))
            (when (< (nth icurrent sorted-list) (nth imin sorted-list))
              (setf imin icurrent))))
        (unless (= imin i)
          (rotatef (nth i sorted-list) (nth imin sorted-list)))))
    sorted-list))

(defun check-imperative (name input expected)
  (format t "~:[FAILED~;PASSED~]... ~a~%"
          (equal (select1-imperative input) expected)
          name))

(defun test-select1-imperative ()
  (check-imperative "test 2.1" '(9 2 5 7 3 0) '(0 2 3 5 7 9))
  (check-imperative "test 2.2" '(6 5 4 3 2 1) '(1 2 3 4 5 6))
  (check-imperative "test 2.3" '(0 1 2 3 4 5) '(0 1 2 3 4 5))
  (check-imperative "test 2.4" '(3 1 2 2 3 1) '(1 1 2 2 3 3))
  (check-imperative "test 2.5" '(5) '(5))
  (check-imperative "test 2.6" '() '()))


