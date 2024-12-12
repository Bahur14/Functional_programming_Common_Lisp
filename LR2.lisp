;Реалізація першої функції
(defun reverse-and-nest-head (lst)
  (if (null lst)  
      nil         
      (if (null (cdr lst)) 
          (list (car lst)) 
          (list (nested-reverse (cdr lst)) (car lst)))))


(defun check-reverse (name input expected)
  (format t "~:[FAILED~;PASSED~]... ~a~%"
          (equal (reverse-and-nest-head input) expected)
          name))

(defun test-reverse-and-nest-head ()
  (check-reverse "test 1.1" '(a b c) '(((c) b) a))
  (check-reverse "test 1.2" '() '())
  (check-reverse "test 1.3" '(1 2 3) '(((3) 2) 1))
  (check-reverse "test 1.4" '(nil 2 3)  '((( 3) 2) nil)))


;Реалізація другої функції
(defun repeat-element (elem times)
  (if (zerop times)
      nil
      (cons elem (repeat-element elem (1- times)))))

(defun duplicate-elements (lst n)
  (if (null lst)
      nil
      (append (repeat-element (car lst) n)
              (duplicate-elements (cdr lst) n))))


(defun check-duplicate (name input n  expected)
  (format t "~:[FAILED~;PASSED~]... ~a~%"
          (equal (duplicate-elements input n) expected)
          name))

(defun test-duplicate-elements ()
  (check-duplicate "test 2.1" '(a b c) 3 '(A A A B B B C C C))
  (check-duplicate "test 2.2" '(1 2 3) 1 '(1 2 3))
  (check-duplicate "test 2.3" '() 5 nil)
  (check-duplicate "test 2.4" '(1 2 3) 0 '()))


