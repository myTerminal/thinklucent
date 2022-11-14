(defpackage :shell
  (:use :cl)
  (:export :execute-in-system))

(defpackage :hardware
  (:use :cl)
  (:import-from :shell
                :execute-in-system)
  (:export :enable-or-disable-lights))

(defpackage :main
  (:use :cl)
  (:import-from :hardware
                :enable-or-disable-lights)
  (:export :main))
