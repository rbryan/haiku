(defpackage :haiku-test
 (:use :cl :prove :haiku :md :files
       :utils :templates :bindings)
 (:export))

(in-package :haiku-test)


(plan 2)

(uiop:with-current-directory ("test/")
  (subtest "Testing File Functions"
    (ok "test.txt" (files:path->filename "/home/test/words/test.txt"))
    (ok "test" (files:strip-extension 
                 (files:path->filename "/home/test/words/test.txt")))
  )
  (subtest "Testing markdown functions"
    (diag "Eventually we need a binding error condition.")
    (is-error (md:load-md #p"body-binding.md") 'simple-error)
))

(finalize)
