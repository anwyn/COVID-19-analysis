(in-package :corona)

;; --------------------------------------------------------------------------------
;; Coronavirus interpretation
;; --------------------------------------------------------------------------------

(defun confirmed-case (corona row)
  (setf (gethash (concatenate 'string (car row) " " (cadr row)) corona)
        (let ((hs (make-hash-table)))
          (loop for x in #1=(nthcdr 4 row)
                and i from 1 to (length #1#)
                do (if (equal x "")  (gethash (- i 1) hs)
                       (setf (gethash i hs)
                             (coerce (parse-integer x) 'double-float))))
          hs)))

(defun inactive-case (corona row)
  (let ((hs (gethash (concatenate 'string (car row) " " (cadr row)) corona)))
    (loop for x in #1=(nthcdr 4 row)
          and i from 1 to (length #1#)
          do (setf (gethash i hs)
                   (if (equal x "")  (gethash i hs)
                       (- (gethash i hs)
                           (coerce (parse-integer x) 'double-float)))))))

(defun daily-cases (cases case-list)
  "daily aggregate"
  (map nil #'(lambda (x)
               (loop for z in (hash-table-alist (cdr x))
                     do (setf (gethash (car z) cases)
                              ;; so either there is no value currently, in which case we use 0
                              (+ (or (gethash (car z) cases) 0)
                                 ;; or
                                 (or (cdr z) 0))))) ;; or use the previous day...

       case-list)
  cases)

(defun rate (data)
  "rate of change"
  (flet ((slope (a b)
           (if (zerop b)
               1
               (- (/ a b) 1))))
    (if (consp (cddr data))
        (cons (slope (cadr data) (car data))
              (rate (cdr data)))
        (cons (slope (cadr data) (car data))
              nil))))

(defun array-to-list (l)
  (loop for x below (car (array-dimensions l))
        collect (loop for a below (cadr (array-dimensions l))
                      collect (aref l x a))))

(defun list-to-array (lst)
  (make-array `(1 ,(length lst))
              :element-type 'double-float
              :initial-contents (list (mapcar #'(lambda (x)
                                               (coerce x 'double-float))
                                           lst))))

(defun polynomial-regression (data coeff)
  "Regression based on coeff."
  (let ((x-values (list-to-array (loop for x from 1 to (length data)
                                     collect (coerce x 'double-float)))))
    (alexandria:flatten (array-to-list (polynomial-fit x-values (list-to-array data) coeff)))))

(defun polynomial-create (l)
  "Creates a closure from a polynomial regression."
  (lambda (x)
    (declare (ignorable x))
    (reduce #'+ (loop for y from 0 to (- (length l) 1) and z in l
                      collect (* z (expt x y))))))

(defun extrapolate (current rate)
  "This should be our sigmoid function actually, but currently we don't know the
value of K, the carrying capacity. For now we're using an expotential growth function,
but with a rate based on our linear regression."
  (* current (expt 2.7182818284590452353602874713527d0 rate)))

(defun extrapolate-cases (cases days)
  "Extrapolate CASES for DAYS"
  (let* ((extrapolated-cases (make-hash-table))
         (slope-regression (polynomial-create
                            (polynomial-regression
                             (rate (hash-table-values cases)) 1)))
         (recent-day (apply #'max (hash-table-keys cases)))
         (var (gethash recent-day cases))
         (rate (lastcar (rate (hash-table-values cases)))))
    (dotimes (x days)
      (let ((d (extrapolate var rate)))
        (setf (gethash (+ x recent-day) extrapolated-cases) d)
        (setf var d
              rate (funcall slope-regression (+ x recent-day)))))
    extrapolated-cases))

;; Setup data
(defun directory-search (x lst)
  (find-if #'(lambda (y)
               (scan x y))
           (mapcar #'namestring lst)))

(defun main (&key (days 90) countries)
  (let ((cases (directory "data/csse_covid_19_data/csse_covid_19_time_series/*.csv"))
        (countries (ensure-list countries))
        (corona (make-hash-table :test 'equal)))

 ;; We want current active cases.
    ;; To get that we have to subtract the number of recovered + deaths
    ;; from the number of conformed cases.
    (with-open-file (s (directory-search "Confirmed" cases))
      (map nil (curry #'confirmed-case corona) (cdr (read-csv s))))

    (with-open-file (s (directory-search "Deaths" cases))
      (map nil (curry #'inactive-case corona) (cdr (read-csv s))))

    (with-open-file (s (directory-search "Recovered" cases))
      (map nil (curry #'inactive-case corona) (cdr (read-csv s))))

    ;; We factor out china for two reasons:
    ;; Ground zero occured in centeral china. They progressed to the later stages
    ;; of an epidemic far before the rest of the world started to see a majority of cases.
    ;; China also has had one of the most effiecient lock-down we've seen, so the spread
    ;; has mostly been contained.

    ;; linear regression of slope of daily active cases
    (let* ((world-cases (daily-cases (make-hash-table :test 'equal)
                                     (remove-if #'(lambda (x) (cl-ppcre:scan "China" (car x)))
                                                (hash-table-alist corona))))
           (ext-world-cases (extrapolate-cases world-cases days)))
      (apply #'plot
             ;; current cases
             (hash-table-keys world-cases)
             (append (hash-table-values world-cases))
             "World"
             ;; extrapolation
             (reverse (hash-table-keys ext-world-cases))
             (reverse (hash-table-values ext-world-cases))
             (format nil "World ~a days" days)
             (mapcan (lambda (country)
                       (when country
                         (let ((country-cases (remove-if-not #'(lambda (x) (cl-ppcre:scan country (car x)))
                                                             (hash-table-alist corona))))
                           (when country-cases
                             (let* ((daily-cases (daily-cases (make-hash-table :test 'equal)
                                                              country-cases))
                                    (ext-cases (extrapolate-cases daily-cases days)))
                               (list (hash-table-keys daily-cases)
                                     (hash-table-values daily-cases)
                                     country
                                     (reverse (hash-table-keys ext-cases))
                                     (reverse (hash-table-values ext-cases))
                                     (format nil "~A ~d days" country days)))))))
                     countries)))))


  ;; (sb-ext:save-lisp-and-die #P"corona" :toplevel #'corona::main :executable t :compression t)

  ;; rates of change
  ;; (hash-table-keys world-cases)
  ;; (rate (hash-table-values world-cases))

;; linear regression of that rate
;; (loop for x from 1 to 60 collect x)
;; (loop for x from 1 to 60 collect (funcall rate-regression x))


;; (defun logistic-growth (current rate time)
;;   ;; capacity is the carrying capacity, the limiting value of N
;;   ;; the inflection point occurs as N = K/2
;;   ;; (x = * (+ 1 n) * p) ;; we know that the rate, p, is something like 1.15
;;   ;; (print (* current (expt 2.718 (* growth time)))))))
;;   ;; b is determined by K / N(0) - 1
;;   (let ((b (derivative (/ (- capacity current) current))))))
;;   ;; In this case we actually are trying to find capacity
;;   ;; I think....
;;   (derivative K (* current (+ 1 (* b (expt 2.718 (* (- rate) time))))))
;;   ;; I think if we can run gradient descent over which interval of t = time
;;   ;; has the steepest increase, then we can start to see when it will hit 0
;;   ;; (/ capacity (+ 1 (* b (expt 2.718 (* (- rate) time)))))
;;   ;; )
;; (gradi (rate (hash-table-values world-cases)))
