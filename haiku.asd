(defpackage :haiku-asd
  (:use :cl :asdf))

(in-package :haiku-asd)


(defsystem haiku
  :name "haiku"
  :version "0.0.0"
  :author "Russell Bryan"
  :license "Simplified BSD License"
  :description "Haiku Static Site Generator"
  :long-description "Haiku: The static site generator for the functional minimalist."

  :in-order-to ((test-op (test-op haiku/test)))

  :depends-on ("split-sequence"
               "cl-mustache"
               "cl-markdown")

  :components ((:file "haiku"
                      :depends-on ("files"
                                   "parameters" 
                                   "md"
                                   "utils"
                                   "templates"))
               (:file "parameters"
                      :depends-on ())
               (:file "bindings"
                      :depends-on ())
               (:file "utils"
                      :depends-on ())
               (:file "files"
                      :depends-on ())
               (:file "templates"
                      :depends-on ("files"
                                   "utils"
                                   "parameters"
                                   "bindings"))
               (:file "md"
                      :depends-on ("files"
                                   "parameters"
                                   "templates"
                                   "utils"
                                   "bindings"))))

(defsystem haiku/test
  :name "haiku/test"
  :version "0.0.0"
  :author "Russell Bryan"
  :license "Simplified BSD License"
  :description "Haiku Static Site Generator Tests"
  :long-description "Haiku Test: Keeping the static site generator for the functional minimalist functional."
  :defsystem-depends-on (:prove-asdf)

  :depends-on ("haiku"
               "prove")

  :components ((:test-file "test/haiku-test"))

  :perform (test-op :after (op c)
             (funcall (intern #.(string :run) :prove) c)))
              

