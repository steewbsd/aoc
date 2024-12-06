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

(defun check-ascending (list firstp)
  (when
      (and
       (not (null (car list)))
       (not (null (cdr list))))
    (let ((increment (abs (- (car list) (cadr list)))))
      (cond
        ((and
          (< (car list) (cadr list))
          (> increment 0) (< increment 4))
         (check-ascending (cdr list) nil))
        ('t (progn
              (format
               t
               "ASC:Test '<' failed on: ~a ~a, dist: ~a ~%" (car list) (cadr list) increment)
              nil))
        )))
  (when
      (and (null (cdr list)) (not firstp))
    (format t "Ascend test succeeded~%")
    t))

(defun check-descending (list firstp)
  (when
      (and
       (not (null (car list)))
       (not (null (cdr list))))
    (let ((decrement (abs (- (car list) (cadr list)))))
      (cond
        ((and
          (> (car list) (cadr list))
          (> decrement 0) (< decrement 4))
         (check-descending (cdr list) nil))
        ('t (progn
              (format
               t
               "DSC:Test '<' failed on: ~a ~a, dist: ~a ~%" (car list) (cadr list) decrement)
              nil))
        )))
  (when
      (and (null (cdr list)) (not firstp))
    (format t "Descend test succeeded~%")
    t))

(defvar table (read-to-list (open "./input") ()))
(defvar parsed-table (mapcar (lambda (x) (mapcar #'parse-integer x)) table))

(defun declare-safe (table)
  (mapcar (lambda (x) (or (check-ascending x 't) (check-descending x 't))) table))

(format t "~a~%" (mapcar (lambda (x) (check-ascending x 't)) parsed-table))
