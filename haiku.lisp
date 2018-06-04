(defpackage :haiku
  (:use :cl :files :md :utils
        :parameters :templates)
  (:export
    :render-md-dir
    :render-with-bindings
    :render-template
))

(in-package :haiku)

(defun render-md-dir (md-dir output-dir &key render-callback initial-bindings verbose)
  (let ((md-dir (uiop:directory-exists-p md-dir))
        (output-dir (uiop:directory-exists-p output-dir)))
    ;add a check that md-dir and output-dir exist
    (assert (and md-dir output-dir))
    (let* ((in-files (files:files-by-extension md-dir ".md"))
           (out-file-dirs (mapcar (lambda (path)
                                (uiop:pathname-directory-pathname
                                  (files:rebase-pathname path md-dir output-dir)))
                              in-files)))
      (uiop:ensure-all-directories-exist out-file-dirs)
      ;add a check that there are md files
      (mapcar (lambda (file out-file-dir)
                (if verbose
                  (format t "Rendering ~a...~%" file))
                (finish-output)
          (let* ((bname (files:basename file))
                 (ofname (format nil "~a/~a.html" out-file-dir bname))
                 (output (md:render-to-file file
                                            ofname
                                            :initial-bindings initial-bindings)))
            (if render-callback
              (funcall render-callback ofname (cdr output))
              output)))
          in-files
          out-file-dirs))))
        
