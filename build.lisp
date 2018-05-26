(defpackage :build
  (:use :cl)
  (:export
    :reload-project))

(in-package :build)

(defun load-project ()
  (format t "Loading dependencies from quicklisp...~%")
  (ql:quickload :split-sequence)
  (ql:quickload :cl-mustache)
  (ql:quickload :cl-markdown)
  
  (format t "Loading files...~%")
  (load "utils.lisp")
  (load "haiku.lisp")
  (load "files.lisp")
  (load "templates.lisp")
  (load "md.lisp")
)

(defmacro reload-project (pkg)
  `(progn (load-project)
          (in-package ,pkg)))

