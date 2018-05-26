(defpackage :parameters
  (:use :cl)
  (:export
    :*template-dir*
    :*markdown-dir*
    :*output-dir*
    :*default-template*
    :*base-template-name*))

(in-package :parameters)

(defparameter *template-dir* "templates")
(defparameter *markdown-dir* "md")
(defparameter *output-dir* "site")
(defparameter *default-template* "<html><body> {{{body}}} </body></html>")
(defparameter *base-template-name* "base.mustache")
