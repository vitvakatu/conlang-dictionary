;   Copyright 2016 Ilya Bogdanov (aka "vitvakatu")
;
;   Licensed under the Apache License, Version 2.0 (the "License");
;   you may not use this file except in compliance with the License.
;   You may obtain a copy of the License at
;
;       http://www.apache.org/licenses/LICENSE-2.0
;
;   Unless required by applicable law or agreed to in writing, software
;   distributed under the License is distributed on an "AS IS" BASIS,
;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;   See the License for the specific language governing permissions and
;   limitations under the License.


(defpackage :vitvakatu.conlang-dict.core
  (:use :cl))
(in-package :vitvakatu.conlang-dict.core)

(defclass literal ()
  ((name 
    :initarg :name
    :initform (error "Need to specify literal's name")
    :accessor name)))

(defclass word ()
  ((literals
    :initarg :literals
    :initform (make-array 0 
                          :fill-pointer 0 
                          :adjustable t
                          :element-type 'literal)
    :accessor literals)))

(defvar *literals* ())

(defun literal= (f s)
  (char= (name f) (name s)))

(defun check-literal (literal)
  (if (find literal *literals* :test 'char=)
      t
      nil))

(defun add-literal (literal)
  (when (null (check-literal literal))
    (setf *literals* (append *literals* (list literal)))))

(defun remove-literal (literal)
  (when (check-literal literal)
    (setf *literals* (remove literal *literals* :test 'char=))))

(defun clear-literals ()
  (setf *literals* ()))

(defun create-word (str)
    (loop for c across str
       append (list (make-instance 
                     'literal 
                     :name c)) into literals
       finally (return 
                 (let ((word (make-instance 'word)))
                   (loop for l in literals do 
                        (vector-push-extend l (literals word)))
                   word))))
                   
