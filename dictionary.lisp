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
    :reader name)))

(defclass word ()
  ((literals
    :initarg :literals
    :initform (make-array 0 
                          :fill-pointer 0 
                          :adjustable t
                          :element-type 'literal)
    :accessor literals)))

(defun word->string (word)
  (loop for l across (literals word) collect (name l) into str
       finally (return (format nil "~{~a~}" str))))

(defvar *literals* ())

(defgeneric literal= (f s))

(defmethod literal= ((f string) (s string))
  (string= f s))

(defmethod literal= ((f literal) (s string))
  (string= (name f) s))

(defmethod literal= ((f string) (s literal))
  (string= f (name s)))

(defmethod literal= ((f literal) (s literal))
  (string= (name f) (name s)))

(defun check-literal (literal)
  (if (find literal *literals* :test 'literal=)
      t
      nil))

(defun add-literal (str)
  (when (null (check-literal str))
    (setf *literals* (append *literals* (list
                                         (make-instance
                                          'literal
                                          :name str))))))

(defun remove-literal (str)
  (when (check-literal str)
    (setf *literals* (remove (make-instance 
                              'literal
                              :name str)
                             *literals* 
                             :test 'literal=))))

(defun clear-literals ()
  (setf *literals* ()))
                   
(defun create-word (str)
  (let ((word (make-instance 'word)))
    (loop for c across str do
         (vector-push-extend (make-instance
                              'literal
                              :name c)
                             (literals word)))
    word))
