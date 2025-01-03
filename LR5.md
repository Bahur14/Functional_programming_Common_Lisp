<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Бахурінський Олександр Дмитрович КВ-12</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання  
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом
 (п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді
CSV файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним
типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.
1. Визначити структури або утиліти для створення записів з таблиць (в залежності від
типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а також якийсь об'єкт, 
який дасть змогу зчитати записи конкретного типу або структури. Це може бути ключ, список з якоюсь 
допоміжною інформацією, функція і т. і. За потреби параметрів може бути кілька. select повертає 
лямбда-вираз, який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було передано 
у select . При цьому лямбда-вираз в якості ключових параметрів може отримати на вхід значення полів 
записів таблиці, для того щоб обмежити вибірку лише заданими значеннями (виконати фільтрування). 
Вибірка повертається у вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від варіанту):
- структури у геш-таблиці
- геш-таблиці у асоціативні списки
- асоціативні списки у геш-таблиці
6. Написати функцію(-ї) для "красивого" виводу записів таблиці.
## Варіант 1
|    База даних    | Тип записів |
|------------------|-------------|
|Виробництво дронів| Структура   |

| Назва | Таблиці  | Опис|
|---|---|---|
| Виробництво дронів | 1. Виробники дронів <br> 2. Дрони| База даних виробників дронів та, власне, дронів.                                                                 |

## Приклад заповнення таблиць
### Companies.cvs
| ID  | Name                   | Country |
| --- | ---------------------- | ------- |
| 1   | Space Robotics         | USA     |
| 2   | DroneTech              | Japan   |
| 3   | AeroCraft Systems      | Germany |
| 4   | SkyGlider Innovations  | UK      |
### Product.csv
| ID  | Manufacturer ID | Model          | Price |
| --- | --------------- | -------------- | ----- |
| 1   | 2               | Nimbus X       | 1200  |
| 2   | 4               | SkySurfer      | 3000  |
| 3   | 1               | Mars Explorer  | 25000 |
| 4   | 3               | AeroScout Pro  | 8500  |
| 5   | 1               | Lunar Surveyor | 50000 |

## Лістинг реалізації завдання
```lisp
(defstruct company
  code
  title
  location)

(defstruct product
  code
  company-code
  name
  cost)        

(defun parse-csv-line (line)
  (let ((delimiter #\,))
    (loop with result = nil
          with current = (make-string-output-stream)
          for char across line
          if (char= char delimiter)
            do (push (string-trim '(#\") (get-output-stream-string current)) result)
               (setf current (make-string-output-stream))
          else
            do (write-char char current)
          finally (push (string-trim '(#\") (get-output-stream-string current)) result)
                  (return (nreverse result)))))

(defun create-company-from-fields (fields)
  (make-company
   :code (parse-integer (first fields))
   :title (second fields)
   :location (third fields)))

(defun create-product-from-fields (fields)
  (make-product
   :code (parse-integer (first fields))
   :company-code (parse-integer (second fields))
   :name (third fields)
   :cost (parse-integer (fourth fields))))

(defun read-table (filepath constructor)
  (with-open-file (stream filepath :direction :input)
    (let ((records nil))
      (read-line stream)
      (loop for line = (read-line stream nil nil)
            while line
            do (push (funcall constructor (parse-csv-line line)) records))
      (nreverse records))))

(defun select (filepath type &rest conditions)
  (let ((reader (case type
                 (:company (lambda () (read-table filepath #'create-company-from-fields)))
                 (:product (lambda () (read-table filepath #'create-product-from-fields)))
                 (otherwise (error "Невідомий тип: ~A" type)))))
    (lambda (&rest filter-values)
      (let ((data (funcall reader)))
        (if conditions
            (remove-if-not
             (lambda (record)
               (every (lambda (condition value)
                       (equal (funcall condition record) value))
                     conditions
                     filter-values))
             data)
            data)))))

(defun record-to-hash-table (record &rest fields)
  (let ((table (make-hash-table :test #'equal)))
    (loop for field in fields
          do (setf (gethash (car field)
                           table)
                  (funcall (cdr field) record)))
    table))

(defun company-to-hash (company)
  (record-to-hash-table company
                        (cons :code #'company-code)
                        (cons :title #'company-title)
                        (cons :location #'company-location)))

(defun product-to-hash (product)
  (record-to-hash-table product
                        (cons :code #'product-code)
                        (cons :company-code #'product-company-code)
                        (cons :name #'product-name)
                        (cons :cost #'product-cost)))

(defun write-records-to-csv (filepath records writer &optional headers filter-fn filter-value)
  (with-open-file (stream filepath
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
    (when headers
      (format stream "~{~A~^,~}~%" headers))
    (let ((filtered-records (if (and filter-fn filter-value)
                               (remove-if-not (lambda (record)
                                              (equal (funcall filter-fn record) filter-value))
                                            records)
                               records)))
      (dolist (record filtered-records)
        (funcall writer stream record)))))

(defun write-company (stream company)
  (format stream "~A\,~A\,~A~%"
          (company-code company)
          (company-title company)
          (company-location company)))

(defun write-product (stream product)
  (format stream "~A\,~A\,~A\,~A~%"
          (product-code product)
          (product-company-code product)
          (product-name product)
          (product-cost product)))

(defun print-header (text)
  (let* ((text-with-spaces (format nil " ~A " text))
         (total-width 80)
         (decoration-width (floor (- total-width (length text-with-spaces)) 2))
         (decoration (make-string decoration-width :initial-element #\=)))
    (format t "~%~A~A~A~%" decoration text-with-spaces decoration)))

(defun print-subheader (text)
  (let* ((text-with-spaces (format nil " ~A " text))
         (total-width 60)
         (decoration-width (floor (- total-width (length text-with-spaces)) 2))
         (decoration (make-string decoration-width :initial-element #\-)))
    (format t "~%~A~A~A~%" decoration text-with-spaces decoration)))

(defun print-record-separator ()
  (format t "~%~A~%" (make-string 60 :initial-element #\-)))

(defun print-table (headers records printer)
  (let* ((col-width 20)
         (total-width (* (length headers) col-width))
         (separator (make-string total-width :initial-element #\-)))

    (format t "~%~A~%" separator)

    (format t "~{~vA~}" (mapcan (lambda (h) (list col-width h)) headers))

    (format t "~%~A~%" separator)

    (dolist (record records)
      (funcall printer record))

    (format t "~A~%" separator)))

(defun print-company (company)
  (format t "~20A~25A~20A~%"
          (company-code company)
          (company-title company)
          (company-location company)))

(defun print-product (product)
  (format t "~20A~20A~20A~20A~%"
          (product-code product)
          (product-company-code product)
          (product-name product)
          (product-cost product)))

```
### Тестові набори та утиліти
```lisp
(defun test-select-function ()
  (format t "~%--- Running Test: test-select ---~%")
    (let* ((select-companies (select "companies.csv" :company #'company-title))
             (filtered-companies (funcall select-companies "DroneTech")))
        (format t "--- Filtered Manufacturers (Title = DroneTech) ---~%")
        (dolist (company filtered-companies)
          (format t "Code: ~A~%Title: ~A~%Locathion: ~A~%------------------------~%"
                  (company-code company)
                  (company-title company)
                  (company-location company))))
    (let* ((select-drones (select "products.csv" :product #'product-company-code))
             (filtered-drones (funcall select-drones 1)))
        (format t "--- Filtered Drones (Company-code = 1) ---~%")
        (dolist (product filtered-drones)
          (format t "Code: ~A~%Name: ~A~%Cost: ~A~%Company-code: ~A~%------------------------~%"
                  (product-code product)
                  (product-name product)
                  (product-cost product)
                  (product-company-code product))))
    (let* ((select-drones (select "products.csv" :product #'product-name))
             (filtered-drones (funcall select-drones "Explorer")))
        (format t "--- Filtered Drones (Name = Explorer) ---~%")
        (dolist (product filtered-drones)
          (format t "Code: ~A~%Name: ~A~%Cost: ~A~%Company-code: ~A~%------------------------~%"
                  (product-code product)
                  (product-name product)
                  (product-cost product)
                  (product-company-code product))))
    (let* ((select-drones (select "products.csv" :product #'product-cost))
             (filtered-drones (funcall select-drones 50000)))
        (format t "--- Filtered Drones (Cost = 50000) ---~%")
        (dolist (product filtered-drones)
          (format t "Code: ~A~%Name: ~A~%Cost: ~A~%Company-code: ~A~%------------------------~%"
                  (product-code product)
                  (product-name product)
                  (product-cost product)
                  (product-company-code product)))))

(defun test-read-from-tables ()
  (format t "~%--- Running Test: test-read-from-tables ---~%")

    (let ((companies (read-table "companies.csv" #'create-company-from-fields)))
        (format t "--- Companies ---~%")
        (dolist (company companies)
          (format t "Code: ~A~%Title: ~A~%Locathion: ~A~%------------------------~%"
                  (company-code company)
                  (company-title company)
                  (company-location company))))
    (let ((products (read-table "products.csv" #'create-product-from-fields)))
        (format t "--- Product ---~%")
        (dolist (product products)
          (format t "Code: ~A~%Name: ~A~%Cost: ~A~%Company-code: ~A~%------------------------~%"
                  (product-code product)
                  (product-name product)
                  (product-cost product)
                  (product-company-code product)))))

(defun test-transform-to-hash ()
  (format t "~%--- Running Test: test-transform-to-hash ---~%")

  (let ((companies (read-table "companies.csv" #'create-company-from-fields)))
    (format t "--- Companies ---~%")
    (dolist (company companies)
      (let ((hash (company-to-hash company)))
        (maphash (lambda (key value)
                   (format t "~A: ~A~%" key value))
                 hash))
      (format t "------------------------~%")))

  (let ((products (read-table "products.csv" #'create-product-from-fields)))
    (format t "--- Product ---~%")
    (dolist (product products)
      (let ((hash (product-to-hash product)))
        (maphash (lambda (key value)
                   (format t "~A: ~A~%" key value))
                 hash))
      (format t "------------------------~%"))))

(defun test-write-to-csv ()
  (print-header "Test: Writing Filtered Data to CSV")

  (print-subheader "Writing Filtered Companies")
  (let* ((companies (read-table "companies.csv" #'create-company-from-fields))
         (output-file "filtered_companies.csv"))
    (write-records-to-csv output-file
                         companies
                         #'write-company
                         '("Code" "Title" "Location")
                         #'company-title
                         "DroneTech")
    (format t "Filtered companies written to: ~A~%" output-file))

  (print-subheader "Writing Filtered Products")
  (let* ((products (read-table "products.csv" #'create-product-from-fields))
         (output-file "filtered_products.csv"))
    (write-records-to-csv output-file
                         products
                         #'write-product
                         '("Code" "Company-code" "Name" "Cost")
                         #'product-cost
                         50000)
    (format t "Filtered products written to: ~A~%" output-file)))

(defun test-pretty-print ()
  (format t "~%--- Running Test: test-pretty-print ---~%")

  (let ((companies (read-table "companies.csv" #'create-company-from-fields)))
    (format t "---------------------- Companies Table ---------------------~%")
    (print-table '("Code" "Title" "Location") companies #'print-company))

  (let ((products (read-table "products.csv" #'create-product-from-fields)))
    (format t "--------------------------------- Product Table --------------------------------~%")
    (print-table '("Code" "Company-code" "Name" "Cost") products #'print-product)))

(defun run-all-tests ()
  (print-header "Starting All Tests")

  (print-header "Testing Table Reading")
  (test-read-from-tables)

  (print-header "Testing Select Function")
  (test-select-function)

  (print-header "Testing Hash Transformation")
  (test-transform-to-hash)

  (print-header "Testing CSV Writing")
  (test-write-to-csv)

  (print-header "Testing Pretty Printing")
  (test-pretty-print)

  (print-header "All Tests Completed"))
```
### Тестування
```lisp
* (RUN-ALL-TESTS)

============================== Starting All Tests ==============================

============================ Testing Table Reading ============================

--- Running Test: test-read-from-tables ---
--- Companies ---
Code: 1
Title: Space Robotics
Locathion: USA
------------------------
Code: 2
Title: DroneTech
Locathion: Japan
------------------------
Code: 3
Title: AeroCraft Systems
Locathion: Germany
------------------------
Code: 4
Title: SkyGlider Innovations
Locathion: UK
------------------------
--- Product ---
Code: 1
Name: Nimbus X
Cost: 1200
Company-code: 2
------------------------
Code: 2
Name: SkySurfer Mars
Cost: 3000
Company-code: 4
------------------------
Code: 3
Name: Explorer
Cost: 25000
Company-code: 1
------------------------
Code: 4
Name: AeroScout Pro
Cost: 8500
Company-code: 3
------------------------
Code: 5
Name: Lunar Surveyor
Cost: 50000
Company-code: 1
------------------------

=========================== Testing Select Function ===========================

--- Running Test: test-select ---
--- Filtered Manufacturers (Title = DroneTech) ---
Code: 2
Title: DroneTech
Locathion: Japan
------------------------
--- Filtered Drones (Company-code = 1) ---
Code: 3
Name: Explorer
Cost: 25000
Company-code: 1
------------------------
Code: 5
Name: Lunar Surveyor
Cost: 50000
Company-code: 1
------------------------
--- Filtered Drones (Name = Explorer) ---
Code: 3
Name: Explorer
Cost: 25000
Company-code: 1
------------------------
--- Filtered Drones (Cost = 50000) ---
Code: 5
Name: Lunar Surveyor
Cost: 50000
Company-code: 1
------------------------

========================= Testing Hash Transformation =========================

--- Running Test: test-transform-to-hash ---
--- Companies ---
CODE: 1
TITLE: Space Robotics
LOCATION: USA
------------------------
CODE: 2
TITLE: DroneTech
LOCATION: Japan
------------------------
CODE: 3
TITLE: AeroCraft Systems
LOCATION: Germany
------------------------
CODE: 4
TITLE: SkyGlider Innovations
LOCATION: UK
------------------------
--- Product ---
CODE: 1
COMPANY-CODE: 2
NAME: Nimbus X
COST: 1200
------------------------
CODE: 2
COMPANY-CODE: 4
NAME: SkySurfer Mars
COST: 3000
------------------------
CODE: 3
COMPANY-CODE: 1
NAME: Explorer
COST: 25000
------------------------
CODE: 4
COMPANY-CODE: 3
NAME: AeroScout Pro
COST: 8500
------------------------
CODE: 5
COMPANY-CODE: 1
NAME: Lunar Surveyor
COST: 50000
------------------------

============================= Testing CSV Writing =============================

====================== Test: Writing Filtered Data to CSV ======================

---------------- Writing Filtered Companies ----------------
Filtered companies written to: filtered_companies.csv

---------------- Writing Filtered Products ----------------
Filtered products written to: filtered_products.csv

=========================== Testing Pretty Printing ===========================

--- Running Test: test-pretty-print ---
---------------------- Companies Table ---------------------

------------------------------------------------------------
Code                Title               Location
------------------------------------------------------------
1                   Space Robotics           USA
2                   DroneTech                Japan
3                   AeroCraft Systems        Germany
4                   SkyGlider Innovations    UK
------------------------------------------------------------
--------------------------------- Product Table --------------------------------

--------------------------------------------------------------------------------
Code                Company-code        Name                Cost
--------------------------------------------------------------------------------
1                   2                   Nimbus X            1200
2                   4                   SkySurfer Mars      3000
3                   1                   Explorer            25000
4                   3                   AeroScout Pro       8500
5                   1                   Lunar Surveyor      50000
--------------------------------------------------------------------------------

============================= All Tests Completed =============================
NIL
```
