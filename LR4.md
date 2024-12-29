<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт до лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>

<p align="right"> 
<b>Студент</b>: 
 Бахурінський Олександр КВ-12</p>

<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
роботи 3 з такими змінами:
- використати функції вищого порядку для роботи з послідовностями (де це
доречно);
- додати до інтерфейсу функції (та використання в реалізації) два ключових
параметра: key та test , що працюють аналогічно до того, як працюють
параметри з такими назвами в функціях, що працюють з послідовностями. При
цьому key має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
варіантом (див. п 4.1.2). Використання псевдо-функцій не забороняється, але, за
можливості, має бути мінімізоване.

## Варіант першої частини (варіант 1)
Алгоритм сортування вибором за незменшенням.
## Лістинг реалізації першої частини завдання
```lisp
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

```
### Тестові набори та утиліти першої частини
```lisp
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
```
### Тестування першої частини
```lisp
CL-USER> (test-select1-functional)
PASSED... Test 1
PASSED... Test 2
PASSED... Test 3
PASSED... Test 4
PASSED... Test 5
PASSED... Test 6
PASSED... Test 7
NIL
```
## Варіант другої частини 1
Написати функцію add-prev-fn , яка має один ключовий параметр — функцію
transform . add-prev-fn має повернути функцію, яка при застосуванні в якості
першого аргументу mapcar разом з одним списком-аргументом робить наступне: кожен
елемент списку перетворюється на точкову пару, де в комірці CAR знаходиться значення
поточного елемента, а в комірці CDR знаходиться значення попереднього елемента
списку. Якщо функція transform передана, тоді значення поточного і попереднього
елементів, що потраплять у результат, мають бути змінені згідно transform .
transform має виконатись мінімальну кількість разів.

```lisp
CL-USER> (mapcar (add-prev-fn) '(1 2 3))
((1 . NIL) (2 . 1) (3 . 2))
CL-USER> (mapcar (add-prev-fn :transform #'1+) '(1 2 3))
((2 . NIL) (3 . 2) (4 . 3))
```
## Лістинг реалізованої програми
```lisp
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
```
### Тестові набори та утиліти
```lisp
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

```
### Тестування
```lisp
CL-USER> (test-add-prev-fn)
PASSED... Test 1
PASSED... Test 2
PASSED... Test 3
PASSED... Test 4
PASSED... Test 5
NIL
```



