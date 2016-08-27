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


(defpackage :vitvaktu.conlang-dict.gui
  (:use :cl :qt)
  (:export :main))
(in-package :conlang-dict)
(named-readtables:in-readtable :qt)

;;;;Hello Qt

(defun main ()
    (let* ((app (make-qapplication))
           (label (#_new QLabel "<strong>Hello Qt!</strong>")))
        (#_setWindowTitle label "Hello, Qt!")
        (#_resize label 100 100)
        (#_show label)
        (unwind-protect
            (#_exec app)
        (#_hide label))))
