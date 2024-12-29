(defun find-min (lst test)
  "Find the smallest value in `lst' using `test'."
  (reduce (lambda (min elem)
            (if (funcall test (cdr elem) (cdr min))
                elem
                min))
          lst))

(defun remove-element (lst elem)
  "Remove the first occurrence of `elem' from `lst'."
  (remove elem lst :count 1 :test (lambda (x y) (eql (car x) (car y)))))

(defun select1-functional (lst test)
  "Sort `lst' using selection sort and `test'."
  (when lst
    (let ((min (find-min lst test)))
      (cons min (select1-functional (remove-element lst min) test)))))

(defun select1 (lst &key (key #'identity) (test #'<))
  (let ((keyed-list (mapcar (lambda (x) (cons x (funcall key x))) lst)))
    (mapcar #'car (select1-functional keyed-list test))))


(defun check-functional (name input expected &key (key #'identity) (test #'<))
  (let ((result (select1 input :key key :test test)))
    (format t "~:[FAILED~;PASSED~]... ~a~%"
            (equal result expected)
            name)
    (when (not (equal result expected))
      (format t "Expected: ~a~%Got: ~a~%~%" expected result))))

(defun test-select1-functional ()
  (check-functional "Test 1" '(5 3 8 1) '(1 3 5 8))
  (check-functional "Test 2" '(5 3 8 1) '(8 5 3 1) :test #'>)
  (check-functional "Test 3" '() '())
  (check-functional "Test 4" '(2 2 2 2) '(2 2 2 2))
  (check-functional "Test 5" '(-5 3 -8 1) '(1 3 -5 -8) :key #'abs)
  (check-functional "Test 6" '(3 -2 1 -4) '(1 -2 3 -4) :key (lambda (x) (* x x)))
  (check-functional "Test 7" '("car" "bicycle" "airplane") '("airplane" "bicycle" "car") :test #'string<))


(format t "~%Testing the function-select1~%")
(test-select1-functional)



(defun add-prev-fn (&key (transform #'identity))
  "Return a function for mapcar to create pairs of current and previous elements."
  (let ((prev nil)       
        (prev-trans nil))
    (lambda (current)
      (let* ((current-trans (funcall transform current)) 
             (result (cons current-trans prev-trans)))  
        (setf prev current
              prev-trans current-trans) 
        result))))

(defun check-add-prev-fn (name input expected &key (transform #'identity))
  (let ((result (mapcar (add-prev-fn :transform transform) input)))
    (format t "~:[FAILED~;PASSED~]... ~a~%" (equal result expected) name)
    (when (not (equal result expected))
      (format t "Expected: ~a~%Got: ~a~%~%" expected result))))

(defun test-add-prev-fn ()

  
  (check-add-prev-fn "Test 1" '(1 2 3) '((1 . NIL) (2 . 1) (3 . 2)))
  (check-add-prev-fn "Test 2" '(10 20 30) '((10 . NIL) (20 . 10) (30 . 20)))

  (check-add-prev-fn "Test 3" '(1 2 3) '((2 . NIL) (3 . 2) (4 . 3)) :transform #'1+)
  (check-add-prev-fn "Test 4" '(1 2 3) '((1 . NIL) (4 . 1) (9 . 4)) :transform (lambda (x) (* x x)))

  (check-add-prev-fn "Test 5" '() '()))

(format t "~%Testing the function add-prev-fn ~%")
(test-add-prev-fn)


  

