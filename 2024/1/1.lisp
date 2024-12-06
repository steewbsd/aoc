(ql:quickload "str")

(defun read-to-list (f car-list cdr-list)
  (let* ((line (read-line f nil nil))
         (parsed-line (str:split " "
                                 (str:collapse-whitespaces line))))
    (cond
      ((and (not (null line)) (not (eq line :eof)))
       (progn
         ;; (format t "Read line: ~a~% ~a~%" n line)
         (read-to-list f
                       (cons (car parsed-line) car-list)
                       (cons (cadr parsed-line) cdr-list))))
      ('t
       (progn
         (format t "Finished reading file")
         (close f)
         (list (reverse car-list) (reverse cdr-list))))
      )))

(defun print-all (list)
  (cond
    ((not (null list)) (progn
                             (format t "~a~%" (car list))
                             (print-all (cdr list))))
    ('t (format t "Finished printing"))
))

(defvar table (read-to-list (open "./input") () ()))
(defvar int-cars (mapcar #'parse-integer (car table)))
(defvar int-cdrs (mapcar #'parse-integer (cadr table)))

(defvar sorted-car (sort int-cars #'<))
(defvar sorted-cdr (sort int-cdrs #'<))

(defvar distances (mapcar (lambda (x y) (abs (- x y))) sorted-car sorted-cdr))

(defvar sum (reduce #'+ distances))
(format t "~a~%" sum)



