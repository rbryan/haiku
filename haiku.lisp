(defpackage :haiku
  (:use :cl :files :md :utils
        :parameters)
  (:export
))

(in-package :haiku)

(defun render-md-dir (md-dir output-dir &optional init-binds)
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
          (let* ((bname (files:basename file))
                 (ofname (format nil "~a/~a.html" out-file-dir bname)))
            (md:render-to-file file ofname init-binds)))
          in-files
          out-file-dirs))))
        
