(defpackage :md
  (:use :cl :cl-markdown :files
        :parameters :templates :utils
        :bindings)
  (:export
    :load-md
    :render-md
    :render-to-file
    ))

(in-package :md)

(defstruct md
  name
  path
  bindings
  body)

;load a markdown struct from a file
(defun load-md (loc)
  (with-open-file (md-stream loc)
    (let ((bindings (bindings:read-bindings md-stream loc)))
      (make-md
        :name
          (files:strip-extension (files:path->filename loc))
        :path
          loc
        :bindings
          bindings
        :body
          (files:read-file-from-stream md-stream)))))

;Render a markdown struct to a string of html
(defmacro md-to-string (markdown)
  (let ((output-stream-symbol (gensym "stream")))
     `(with-output-to-string (,output-stream-symbol)
        (markdown ,markdown
                  :stream ,output-stream-symbol))))

;Find a template for a markdown struct
;falling back to the base-template-name if no template
;is found in the header. If there's no corresponding file
;then fall back to the default template
(defun get-md-template (md)
  (let ((template-binding (binding-assoc 'template (md-bindings md)))
        (base-template-loc (in-template-dir parameters:*base-template-name*)))
;    (format t "md bindings: ~a~%" (md-bindings md))
;    (format t "Template binding: ~a~%" template-binding)
    (cond
      (template-binding
          (load-template (in-template-dir (cdr template-binding))))
      ((uiop:probe-file* base-template-loc)
          (load-template base-template-loc))
      (t
        (make-template
          :name
            "*default-template*"
          :body
            parameters:*default-template*)))))

(defun add-bindings (new old)
  ;remove 'extends so that we don't shadow later extends
  (let ((old (remove-if (lambda (b) (symbol-equal 'extends (car b))) old)))
    (append old new)))

;load a markdown file
;
;markdown->html
;
;find associated template or default
; 1)check for template value in header
; 2)base.mustache
; 3)minimal template
;
;render template with body set to markdown html and bindings set to
;the template bindings appended to the markdown bindings
;
;if the template extended another template, render that template with
;body set to the generated html and bindings again appended. Remember to remove
;old body and extends values from bindings so that new ones are used.
;
;Values closer to the markdown will shadow higher up values by being first in the alist

(defun render-md (md &optional initial-bindings)
  (let* ((md-temp (get-md-template md))
         (md-binds (add-bindings (md-bindings md) initial-bindings))
         (bindings (add-bindings (template-bindings md-temp)
                                 md-binds)))
    (render-with-bindings
      (mustache:render* (template-body md-temp)
                        (append `((body . ,(md-to-string (md-body md))))
                                 bindings))
      bindings)))

(defun render-with-bindings (html bindings)
  (let ((template (let ((tb (binding-assoc 'extends bindings)))
                    (if tb
                      (progn ;(format t "Using template ~a~%" (cdr tb))
                      (load-template (in-template-dir (cdr tb))))))))
    (if template
      (render-with-bindings
        (mustache:render* (template-body template)
                          (append  `((body . ,html)) bindings))
        (add-bindings (template-bindings template) bindings))
      html)))

(defun render-to-file (mdf ofname &optional init-binds)
  (with-open-file (ofs ofname :if-exists :supersede :if-does-not-exist :create :direction :output)
    (format ofs "~a" (render-md (load-md mdf) init-binds))))
