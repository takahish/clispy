;;;; Copyright 2019 Takahiro Ishikawa. All Rights Reserved.
;;;;
;;;; Licensed under the Apache License, Version 2.0 (the "License");
;;;; you may not use this file except in compliance with the License.
;;;; You may obtain a copy of the License at
;;;;
;;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing, software
;;;; distributed under the License is distributed on an "AS IS" BASIS,
;;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;; See the License for the specific language governing permissions and
;;;; limitations under the License.
;;;; ==========================================================================

(in-package "COMMON-LISP")


;;; Defines functions, macros or classes.

(defun caar (x) (car (car x)))

(defun cadr (x) (car (cdr x)))

(defun cdar (x) (cdr (car x)))

(defun cddr (x) (cdr (cdr x)))

(defun caaar (x) (car (car (car x))))

(defun caadr (x) (car (car (cdr x))))

(defun cadar (x) (car (cdr (car x))))

(defun caddr (x) (car (cdr (cdr x))))

(defun cdaar (x) (cdr (car (car x))))

(defun cdadr (x) (cdr (car (cdr x))))

(defun cddar (x) (cdr (cdr (car x))))

(defun cdddr (x) (cdr (cdr (cdr x))))

(defun caaaar (x) (car (car (car (car x)))))

(defun caaadr (x) (car (car (car (cdr x)))))

(defun caadar (x) (car (car (cdr (car x)))))

(defun caaddr (x) (car (car (cdr (cdr x)))))

(defun cadaar (x) (car (cdr (car (car x)))))

(defun cadadr (x) (car (cdr (car (cdr x)))))

(defun caddar (x) (car (cdr (cdr (car x)))))

(defun cadddr (x) (car (cdr (cdr (cdr x)))))

(defun cdaaar (x) (cdr (car (car (car x)))))

(defun cdaadr (x) (cdr (car (car (cdr x)))))

(defun cdadar (x) (cdr (car (cdr (car x)))))

(defun cdaddr (x) (cdr (car (cdr (cdr x)))))

(defun cddaar (x) (cdr (cdr (car (car x)))))

(defun cddadr (x) (cdr (cdr (car (cdr x)))))

(defun cdddar (x) (cdr (cdr (cdr (car x)))))

(defun cddddr (x) (cdr (cdr (cdr (cdr x)))))


;;; Exports symbols.

(export '(caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr
          cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
          cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr))


;;; Inherits symbols from common-lisp.

(use-package 'common-lisp 'common-lisp-user)
