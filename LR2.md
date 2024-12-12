<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт до лабораторної роботи 2</b><br/>
"Рекурсія"<br/>
дисципліни "Вступ до функціонального програмування"
</p>

<p align="right"> 
<b>Студент</b>: 
 Бахурінський Олександр КВ-12</p>

<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
Реалізуйте дві рекурсивні функції, що виконують деякі дії з вхідним(и) списком(-ами), за
можливості/необхідності використовуючи різні види рекурсії. Функції, які необхідно
реалізувати, задаються варіантом (п. 2.1.1). Вимоги до функцій:
1. Зміна списку згідно із завданням має відбуватись за рахунок конструювання нового
списку, а не зміни наявного (вхідного).
2. Не допускається використання функцій вищого порядку чи стандартних функцій
для роботи зі списками, що не наведені в четвертому розділі навчального
посібника.
3. Реалізована функція не має бути функцією вищого порядку, тобто приймати функції
в якості аргументів.
4. Не допускається використання псевдофункцій (деструктивного підходу).
5. Не допускається використання циклів.
Кожна реалізована функція має бути протестована для різних тестових наборів. Тести
мають бути оформленні у вигляді модульних тестів (див. п. 2.3).

## Варіант 8

   1.Написати функцію reverse-and-nest-head , яка обертає вхідний список та утворює
вкладeну структуру з підсписків з його елементами, починаючи з голови:
```lisp
CL-USER> (reverse-and-nest-head '(a b c))
 (((C) B) A)
```
2.Написати функцію duplicate-elements , що дублює елементи вхідного списку
задану кількість разів:
```lisp
CL-USER> (duplicate-elements '(a b c) 3)
(A A A B B B C C C)
```

## Лістинг функції reverse-and-nest-head
```lisp
(defun reverse-and-nest-head (lst)
  (if (null lst)  
      nil         
      (if (null (cdr lst)) 
          (list (car lst)) 
          (list (nested-reverse (cdr lst)) (car lst)))))
```
### Тестові набори
```lisp
(defun check-reverse (name input expected)
  (format t "~:[FAILED~;PASSED~]... ~a~%"
          (equal (reverse-and-nest-head input) expected)
          name))

(defun test-reverse-and-nest-head ()
  (check-reverse "test 1.1" '(a b c) '(((c) b) a))
  (check-reverse "test 1.2" '() '())
  (check-reverse "test 1.3" '(1 2 3) '(((3) 2) 1))
  (check-reverse "test 1.4" '(nil 2 3)  '((( 3) 2) nil)))
```
### Тестування
```lisp
CL-USER> (test-reverse-and-nest-head)
PASSED... test 1.1
PASSED... test 1.2
PASSED... test 1.3
PASSED... test 1.4
NIL
```
## Лістинг функції duplicate-element
```lisp
(defun repeat-element (elem times)
  (if (zerop times)
      nil
      (cons elem (repeat-element elem (1- times)))))

(defun duplicate-elements (lst n)
  (if (null lst)
      nil
      (append (repeat-element (car lst) n)
              (duplicate-elements (cdr lst) n))))
```
### Тестові набори
```lisp
(defun check-duplicate (name input n  expected)
  (format t "~:[FAILED~;PASSED~]... ~a~%"
          (equal (duplicate-elements input n) expected)
          name))

(defun test-duplicate-elements ()
  (check-duplicate "test 2.1" '(a b c) 3 '(A A A B B B C C C))
  (check-duplicate "test 2.2" '(1 2 3) 1 '(1 2 3))
  (check-duplicate "test 2.3" '() 5 nil)
  (check-duplicate "test 2.4" '(1 2 3) 0 '()))
```
### Тестування
```lisp
CL-USER> (test-duplicate-elements)
PASSED... test 2.1
PASSED... test 2.2
PASSED... test 2.3
PASSED... test 2.4
```

