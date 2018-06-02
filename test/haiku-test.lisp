(defpackage :haiku-test
 (:use :cl :prove :haiku :md :files
       :utils :templates :bindings)
 (:export)
 (:shadow :is-error))

(in-package :haiku-test)

(defmacro is-error (form)
  `(prove:is-error ,form 'simple-error))

(plan 3)

(uiop:with-current-directory ("test/")
  (subtest "Testing Bindings Functions"
    (ok (bindings:symbol-equal 'utils::a 'bindings::a))
    (let* ((bindings '((a . "1") (b . "2")))
           (err-body-bindings (append bindings '((body . "should error"))))
           (err-double-template-bindings (append bindings 
                                                 '((template . "should error")
                                                   (template . "no really"))))
           (err-double-extends-bindings (append bindings 
                                                 '((extends . "should error")
                                                   (extends . "no really")))))
      (ok (not (bindings:binding-assoc 'utils::c bindings)))
      (ok (bindings:binding-assoc 'utils::b bindings))
      (pass (bindings:check-bindings bindings ""))
      (is-error (bindings:check-bindings err-body-bindings ""))
      (is-error (bindings:check-bindings err-double-template-bindings ""))
      (is-error (bindings:check-bindings err-double-extends-bindings "")))
  )
  (subtest "Testing File Functions"
    (is "test.txt" (files:path->filename "/home/test/words/test.txt"))
    (is "test" (files:strip-extension 
                 (files:path->filename "/home/test/words/test.txt")))
  )
  (subtest "Testing markdown functions"
    (diag "Eventually we need a binding error condition.")
    (is-error (md:load-md #p"body-binding.md"))
))

(finalize)
