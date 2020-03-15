;;; -*- lisp -*-

(asdf:defsystem :corona
  :description "covid-19 analytics"
  :long-description "Analyse CSSE timeseries data of corona virus cases."
  :version "0.1"
  :author "Jake LaBossier <jakelaboss@gmail.com>"
  :licence ""
  :depends-on (:inferior-shell :cl-csv :vgplot :cl-ppcre)
  :components ((:doc-file "README.md")
               (:static-file "corona.asd")
               (:file "package")
               (:file "polynomial-regression" :depends-on ("package"))
               (:file "regression" :depends-on ("polynomial-regression"))))
