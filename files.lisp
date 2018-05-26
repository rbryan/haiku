(defpackage :files
  (:use :cl :split-sequence)
  (:export
    :files-by-extension
    :read-file-from-stream
    :path->filename
    :strip-extension
    ))

(in-package :files)

(defun files-by-extension (dir ext)
  (let ((collection '()))
    (uiop:collect-sub*directories 
      dir
      (lambda (dir) t)
      (lambda (dir) t)
      (lambda (dir)
        (setf collection (append (uiop:directory-files dir (format nil "*~a" ext))
			         collection))))
    collection))

(defun read-file-from-stream (stream)
  (coerce
    (loop
      for c = (read-char stream nil) then (read-char stream nil)
      while c collecting c )
    'string))

;;If you'd like backslashes etc in your filenames
;;you can re-write this yourself. I'd also strongly
;;recommend talking to someone about whatever trauma
;;occurred in your past that's made you this way.
(defun path->filename (path)
  (let* ((sep #\/)
         (split-path (split-sequence sep path)))
         (first (last split-path))))

;;If you'd like #\.'s in your filenames
;;you can re-write this yourself you absolute nut.
;;You probably like #\\'s in your file names too.
(defun strip-extension (file-name)
  (first (split-sequence #\. file-name)))
