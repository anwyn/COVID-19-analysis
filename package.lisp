;;; package.lisp --- package definitions for the corona system
;;;

(in-package :cl-user)

(defpackage #:corona
  (:use #:cl)
  (:import-from #:alexandria
                #:flatten
                #:hash-table-alist
                #:hash-table-keys
                #:hash-table-values
                #:lastcar
                #:curry
                #:ensure-list)
  (:import-from #:cl-csv
                #:read-csv
                #:row)
  (:import-from #:cl-ppcre
                #:scan)
  (:import-from #:inferior-shell
                #:run/s)
  (:import-from #:vgplot
                #:plot)
  (:export #:main))


;;; package.lisp ends here
