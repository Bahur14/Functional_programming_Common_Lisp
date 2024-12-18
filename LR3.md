<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт до лабораторної роботи 3</b><br/>
"Конструктивний і деструктивний підходи до роботи зі списками""<br/>
дисципліни "Вступ до функціонального програмування"
</p>

<p align="right"> 
<b>Студент</b>: 
 Бахурнінський Олександр КВ-12</p>

<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і
імперативно.
1. Функціональний варіант реалізації має базуватись на використанні рекурсії і
конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного списку.
Не допускається використання: псевдо-функцій, деструктивних операцій, циклів,
функцій вищого порядку або функцій для роботи зі списками/послідовностями, що
використовуються як функції вищого порядку. Також реалізована функція не має
бути функціоналом (тобто приймати на вхід функції в якості аргументів).
2. Імперативний варіант реалізації має базуватись на використанні циклів і
деструктивних функцій (псевдофункцій). Не допускається використання функцій
вищого порядку або функцій для роботи зі списками/послідовностями, що
використовуються як функції вищого порядку. Тим не менш, оригінальний список
цей варіант реалізації також не має змінювати, тому перед виконанням
деструктивних змін варто застосувати функцію copy-list (в разі необхідності).
Також реалізована функція не має бути функціоналом (тобто приймати на вхід
функції в якості аргументів).

Алгоритм, який необхідно реалізувати, задається варіантом (п. 3.1.1). Зміст і шаблон звіту
наведені в п. 3.2.

Кожна реалізована функція має бути протестована для різних тестових наборів. Тести
мають бути оформленні у вигляді модульних тестів (наприклад, як наведено у п. 2.3).

## Варіант 1
   Алгоритм сортування вибором за незменшенням.

## Лістинг функції з використанням конструктивного підходу
```lisp

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
```
### Тестові набори та утиліти
```lisp
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
```
### Тестування
```lisp
CL-USER> (test-select1-functional)
PASSED... test 1.1
PASSED... test 1.2
PASSED... test 1.3
PASSED... test 1.4
PASSED... test 1.5
PASSED... test 1.6
NIL
```
## Лістинг функції з використанням деструктивного підходу
```lisp
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
```
### Тестові набори та утиліти
```lisp
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

```
### Тестування
```lisp
CL-USER> (test-select1-imperative)
PASSED... test 2.1
PASSED... test 2.2
PASSED... test 2.3
PASSED... test 2.4
PASSED... test 2.5
PASSED... test 2.6
NIL
```


