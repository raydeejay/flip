;;;; flip.lisp

(in-package #:flip)

;; TODO - finish this toplevel interface
(defun main (filename)
  "Creates a VM, loads a program from a file and runs it."
  (let ((vm (make-instance 'microvm)))
    (load-program vm filename)
    (run vm)))
