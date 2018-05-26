(defpackage :haiku-test
 (:use :cl :prove :haiku :md :files
       :utils :templates :bindings)
 (:export))

(in-package :haiku-test)


(plan 1)

(is 1 1)

(finalize)
