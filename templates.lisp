(defpackage :templates
  (:use :cl :mustache :files :utils :bindings :parameters)
  (:export
    :load-template
    :make-template
    :template-body
    :template-bindings
    :template-location
    :template-name
    :render-template
    :in-template-dir))

(in-package :templates)

(defstruct template
  name
  location
  bindings
  body)
      
(defun load-template (loc)
  (with-open-file (template-stream loc)
    (let ((bindings (bindings:read-bindings template-stream loc)))
      (make-template
        :bindings
          bindings
        :body
          (files:read-file-from-stream template-stream)))))

(defun in-template-dir (template-name)
(concatenate 'string
              parameters:*template-dir*
              "/"
              template-name))

(defun render-template (template body bindings)
      (mustache:render* (template-body template)
                        (append `((body . ,body))
                                 bindings)))
