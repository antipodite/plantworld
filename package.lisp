(defpackage :plantworld
  (:use :common-lisp :cl-hash-util)
  (:export :run :plant)
  (:import-from :alexandria #:curry #:flatten))
