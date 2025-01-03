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
