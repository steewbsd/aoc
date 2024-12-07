(ql:quickload "str")

(defun read-to-list (f table)
  (let* ((line (read-line f nil nil))
         (parsed-line (str:split " "
                                 (str:collapse-whitespaces line))))
    (cond
      ((and (not (null line)) (not (eq line :eof)))
       (progn
         ;; (format t "Read line: ~a~% ~a~%" n line)
         (read-to-list f
                       (append table (list parsed-line)))))
      ('t
       (progn
         (format t "Finished reading file~%")
         (close f)
         table))
      )))

;;; Checks whether two numbers in a sequence fullfill the requirements of:
;;; - Ascending, - Descending, - Step higher than 0, - Step lower than 4
(defun check-sequence (a b)
  (let* ((step (- b a))
         (step-abs (abs step)))
    (cond ((and (> step-abs 0) (< step-abs 4))
           (cond ((plusp step) 'ascending)
                 ((minusp step) 'descending)))
          (t (progn
               ;; (format t "Reached invalid range! ~a~%" step)
               'invalid-step)))))

;;; Applies check-sequence for a line
(defun line-sequence (line sequences)
  ;; Apply recursively
  (if (cdr line)
      (progn
        ;(format t "~a~%" sequences)    
        (line-sequencep 
         (cdr line) 
         (append sequences (list (check-sequence (car line) (cadr line))))))
      sequences
      ))

;;; Check if all elements are ascending or descending
(defun sequencep (line)
  ;; (format t "~a~%" line)
  (or 
   (every (lambda (x) (equal x 'ascending)) line)
   (every (lambda (x) (equal x 'descending)) line))
  )

(defun declare-safe (table)
  (let ((applied-sequence (mapcar (lambda (x) (line-sequencep x ())) table)))
    ;; (format t "~a~%" applied-sequence)
    (mapcar #'sequencep applied-sequence)))

(defvar table (read-to-list (open "./input") ()))
(defvar parsed-table (mapcar (lambda (x) (mapcar #'parse-integer x)) table))


(format t "~a~%" (length (remove-if #'null (declare-safe parsed-table))))
