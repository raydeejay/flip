;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:flip)

;; https://users.ece.cmu.edu/~koopman/stack_computers/sec4_2.html


(defclass vm ()
  ((memory        :accessor memory        :initform (make-array '(#xffff) :initial-element nil))
   (free          :accessor free          :initform 0)
   (ip            :accessor ip            :initform 0)
   (stack         :accessor stack         :initform nil)
   (dictionary    :accessor dictionary    :initform nil)
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
;; the dictionary holds lists of (symbol, memory-address, length)

;; the code (a sequence of symbols, literals, or functions) is held in
;; memory beginning at the address specified in the dictionary entry,
;; one element in each memory position

;; execution of a word consists in getting the address and length, and
;; execute from memory in order

;; DO
;; push address, max, and count into the loop stack

;; execute instruction at address IP in the memory

;; LOOP
;; increment count
;; f count < max then set the IP to the address stored by DO


(defmacro add-primitive (name &body body)
  ;; get the first available position in memory
  `(let ((word-address (free vm)))
     ;; put the code, one token at a time, in memory
     (setf (elt (memory vm) (free vm)) (lambda () ,@body))
     ;; update the first available position
     (incf (free vm))
     ;; add the entry in the dictionary
     (push (list ',name word-address 1)
           (dictionary vm))))

(defmacro add-word (name &body body)
  ;; get the first available position in memory
  `(loop :with word-address := (free vm)
      ;; put the code, one token at a time, in memory
      :for token :in ',body :doing
      (setf (elt (memory vm) (free vm)) token)
      ;; update the first available position
      (incf (free vm))
      ;; count the bytes
      :summing 1 :into length
      ;; add the entry in the dictionary
      :finally (push (list ',name word-address length)
                     (dictionary vm))))

(defmacro add-word% (name &body body)
  ;; get the first available position in memory
  `(loop :with word-address := (free vm)
      ;; put the code, one token at a time, in memory
      :for token :in ,@body :doing
      (setf (elt (memory vm) (free vm)) token)
      ;; update the first available position
      (incf (free vm))
      ;; count the bytes
      :summing 1 :into length
      ;; add the entry in the dictionary
      :finally (push (list ,name word-address length)
                     (dictionary vm))))

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
                        (add-word% (car code) (cdr code))
                        (setf (compiled-code vm) nil)
                        (setf (compiling? vm) nil)))

  ;; initial dictionary - compound words
  ;; load these from file?
  (add-word sq dup *))


;; compile/execute
(defun interpret-word (vm symbol &optional dict)
  (multiple-value-bind (header visible-dict) (find-word vm symbol dict)
    (when (null header) (error "Word ~A not found in the dictionary." symbol))
    (destructuring-bind (word address length) header
      (declare (ignore word))
      (setf (ip vm) address)
      (loop :while (< (ip vm) (+ address length))
         :do (let ((token (elt (memory vm) (ip vm)))
                   (ip (ip vm)))        ; preserve the IP
               (cond ((numberp token)
                      (push token (stack vm)))
                     ((functionp token)
                      (funcall token))
                     ((symbolp token)
                      (interpret-word vm token visible-dict)))
               (setf (ip vm) ip) ; and restore it after a potential call
               (incf (ip vm)))))))      ; then increment it

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
  (cond ((numberp token)
         (push token (compiled-code vm)))
        ((member token *immediate-words* :test #'equal)
         ;; assume that all immediates are primitives (ugh...?)
         (funcall (elt (memory vm) (cadr (find-word vm token)))))
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
