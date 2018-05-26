(defpackage :bindings
  (:use :cl)
  (:export
    :read-bindings
    :check-bindings
    :symbol-equal
    :binding-assoc))


(in-package :bindings)

;;We need a separate equality
;;test for symbols in bindings
;;because they're going to exist in
;;the package that the read function was
;;called in.
(defun symbol-equal (a b)
  (equalp (symbol-name a) (symbol-name b)))

;;assoc macro using our custom equality function.
(defmacro binding-assoc (sym bindings)
  `(assoc ,sym ,bindings :test #'bindings:symbol-equal))

;;Read in bindings from stream and check for common mistakes
(defun read-bindings (strm loc)
  (let ((bindings (read strm)))
    (if (not (listp bindings))
      (error "Possibly missing header in ~a~%" loc))
    (check-bindings bindings loc)
    bindings))

;;Check for common mistakes in bindings including
;;  *double bindings to template or extends
;;  *bindings to body
(defun check-bindings (bindings loc)
  (if (binding-assoc 'body bindings)
    (error "Binding to the keyword 'body' in ~a~%" loc))
  (let ((template-bindings
          (loop for binding in bindings
                when (symbol-equal 'template (car binding))
                collecting binding))
        (extends-bindings
          (loop for binding in bindings
                when (symbol-equal 'extends (car binding))
                collecting binding)))
    (if (< 1 (length template-bindings))
      (error "Multiple bindings to keyword 'template' in ~a~%" loc))

    (if (< 1 (length extends-bindings))
      (error "Multiple bindings to keyword 'extends' in ~a~%" loc))))

