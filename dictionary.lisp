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

(defvar *letters* ())

(defun check-letter (letter)
  (if (find letter *letters* :test 'char=)
      t
      nil))

(defun add-letter (letter)
  (when (null (check-letter letter))
    (setf *letters* (append *letters* (list letter)))))

(defun remove-letter (letter)
  (when (check-letter letter)
    (setf *letters* (remove letter *letters* :test 'char=))))

(defun clear-letters ()
  (setf *letters* ()))

