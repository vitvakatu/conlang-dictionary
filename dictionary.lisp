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
    :accessor literals)
   (translations
    :initarg :translations
    :accessor translations)))

(defclass dictionary ()
  ((words
    :initarg :words
    :initform (make-array 0
                          :fill-pointer 0
                          :adjustable t
                          :element-type 'word)
    :accessor words)))

(defun word->string (word)
  (loop for l across (literals word) collect (name l) into str
       finally (return (format nil "~{~a~}" str))))

(defun trans->string (word)
  (loop for tr in (translations word) collect tr into str
       finally (return (format nil "~{~a~^, ~}" str))))

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

(defgeneric word= (f s))

(defmethod word= ((f string) (s string))
  (string= f s))

(defmethod word= ((f word) (s string))
  (string= (word->string f) s))

(defmethod word= ((f string) (s word))
  (string= f (word->string s)))

(defmethod word= ((f word) (s word))
  (string= (word->string f) (word->string s)))

(defun check-literal (literal)
  (if (find literal *literals* :test #'literal=)
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
                             :test #'literal=))))

(defun clear-literals ()
  (setf *literals* ()))
                   
(defun create-word (str trans)
  (let ((word (make-instance 'word)))
    (loop for c across str do
         (vector-push-extend (make-instance
                              'literal
                              :name c)
                             (literals word)))
    (setf (translations word) trans)
    word))

(defun check-word (word dictionary)
  (if (find word (words dictionary) :test #'word=)
      t
      nil))

(defun add-word (word dictionary)
  (when (null (check-word word dictionary))
    (vector-push-extend word (words dictionary))))

(defun remove-word (word dictionary)
  (when (check-word word dictionary)
    (remove word (words dictionary) :test #'word=)))

(defun replace-translations (word trans)
  (setf (translations word) trans))
