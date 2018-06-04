;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:flip)

;; https://users.ece.cmu.edu/~koopman/stack_computers/sec4_2.html



;; IP is the forth's instruction pointer
;; PC is the machine's program counter
(defclass vm ()
  ((memory        :accessor memory        :initform (make-array '(#xffff) :element-type '(unsigned-byte 8)))
   (stack         :accessor stack         :initform nil)
   (dictionary    :accessor dictionary    :initarg  :dictionary :initform nil)
   (compiling?    :accessor compiling?    :initform nil)
   (compiled-code :accessor compiled-code :initform nil)
   (input         :accessor input         :initform nil))
  (:documentation "A virtual machine."))

;; dictionary operation
(defun find-word (vm symbol &optional dict)
  (let* ((visible-dict (member symbol (or dict (dictionary vm)) :key #'car))
         (word (car visible-dict)))
    (values word visible-dict)))


;; dictionary manipulation
(defmacro add-primitive (name &body body)
  `(push (list ',name (lambda () ,@body))
         (dictionary vm)))

(defmacro add-word (name &body body)
  `(push (cons ',name ',body)
         (dictionary vm)))

(defmacro add-word% (name body)
  `(push (cons ,name ,body)
         (dictionary vm)))

(defmacro forget-word (name)
  `(let ((visible-dict (nth-value 1 (find-word vm ',name))))
     (setf (dictionary vm) (cdr visible-dict))))

(defmacro forget-word% (name)
  `(let ((visible-dict (nth-value 1 (find-word vm ,name))))
     (setf (dictionary vm) (cdr visible-dict))))


;; init vm and dictionary
(defmethod init ((vm vm))
  (setf (stack vm) (list)
        (compiling? vm) nil
        (compiled-code vm) nil
        (input vm) nil)
  ;; initial dictionary - primitives
  (add-primitive dup  (push (car (stack vm)) (stack vm)))
  (add-primitive drop (pop (stack vm)))
  (add-primitive over (push (cadr (stack vm)) (stack vm)))
  (add-primitive rot  (let* ((a (pop (stack vm)))
                             (b (pop (stack vm)))
                             (c (pop (stack vm))))
                        (push b (stack vm))
                        (push a (stack vm))
                        (push c (stack vm))))
  (add-primitive swap (let* ((a (pop (stack vm)))
                             (b (pop (stack vm))))
                        (push a (stack vm))
                        (push b (stack vm))))
  (add-primitive -    (let* ((a (pop (stack vm)))
                             (b (pop (stack vm))))
                        (push (- b a) (stack vm))))
  (add-primitive +    (push (+ (pop (stack vm)) (pop (stack vm)))
                            (stack vm)))
  (add-primitive *    (push (* (pop (stack vm)) (pop (stack vm)))
                            (stack vm)))
  (add-primitive /    (multiple-value-bind (q r) (floor (pop (stack vm)) (pop (stack vm)))
                        (push r (stack vm))
                        (push q (stack vm))))
  (add-primitive =    (push (if (= (pop (stack vm)) (pop (stack vm))) #xffff 0)
                            (stack vm)))
  (add-primitive <>   (push (if (/= (pop (stack vm)) (pop (stack vm))) #xffff 0)
                            (stack vm)))
  (add-primitive <    (let* ((a (pop (stack vm)))
                             (b (pop (stack vm))))
                        (push (if (< b a) #xffff 0) (stack vm))))
  (add-primitive >    (let* ((a (pop (stack vm)))
                             (b (pop (stack vm))))
                        (push (if (> b a) #xffff 0) (stack vm))))
  (add-primitive .n   (print (pop (stack vm))))
  (add-primitive emit (princ (code-char (pop (stack vm)))))

  ;; (add-primitive forget (let ((word (pop (input vm))))
  ;;                         (pop (gethash word (dictionary vm)))))

  ;; "magic" words
  (add-primitive \:   (setf (compiling? vm) T))
  (add-primitive \;   (let* ((code (reverse (compiled-code vm))))
                        (break)
                        (add-word% (car code) (cdr code))
                        (setf (compiled-code vm) nil)
                        (setf (compiling? vm) nil)))

  ;; initial dictionary - compound words
  ;; load these from file?
  (add-word sq dup *))



;; compile/execute
(defun interpret-word (vm symbol &optional dict)
  (multiple-value-bind (word visible-dict) (find-word vm symbol dict)
    (when (null word) (error "Word ~A not found in the dictionary." symbol))
    (loop :for token :in (cdr word)
       :do (cond ((numberp token)
                  (push token (stack vm)))
                 ((functionp token)
                  (funcall token))
                 ((symbolp token)
                  (interpret-word vm token visible-dict))))))

(defmethod execute ((vm vm) token)
  (cond ((numberp token)
         (push token (stack vm)))
        ((find-word vm token)
         (interpret-word vm token (nth-value 1 (find-word vm token))))
        ((functionp token)
         (funcall token))
        (t (error "Something went wrong executing, dude!"))))

(defparameter *immediate-words* '(forget \;))

(defmethod compile* ((vm vm) token)
  (break)
  (cond ((numberp token)
         (push token (compiled-code vm)))
        ((member token *immediate-words* :test #'equal)
         ;; assume that all immediates are primitives (ugh...)
         (funcall (cadr (find-word vm token))))
        ((find-word vm token)
         (push token (compiled-code vm)))
        ((functionp token)
         (push token (compiled-code vm)))
        (t (error "Something went wrong compiling, dude!"))))

(defmethod interpret ((vm vm) input)
  (loop
     :for token := (pop input)
     :while token

     :if (compiling? vm)
     :do (if (compiled-code vm)
             (compile* vm token)
             (push token (compiled-code vm)))
     :else
     :do (case token
           (forget (let ((word (pop input)))
                     (forget-word% word)))
           (otherwise (execute vm token)))
     :end))


;; REPL
(defmethod repl ((vm vm))
  (loop :for input := (read-from-string (concatenate 'string "(" (read-line) ")"))
     :while (not (equal input '(.quit)))
     :do (progn (setf (input vm) input)
                (interpret vm (input vm)))))
