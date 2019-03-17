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

(defun list-length (lst)
  (if (eql lst nil)
      0
      (+ 1 (list-length (cdr lst)))))

(defun nth (n lst)
  (labels ((ith (i lst)
             (if (= i n)
                 (car lst)
                 (ith (+ i 1) (cdr lst)))))
    (ith 0 lst)))

(defun first (lst) (car lst))

(defun second (lst) (cadr lst))

(defun third (lst) (caddr lst))

(defun fourth (lst) (cadddr lst))

(defun fifth (lst) (nth 4 lst))

(defun sixth (lst) (nth 5 lst))

(defun seventh (lst) (nth 6 lst))

(defun eighth (lst) (nth 7 lst))

(defun ninth (lst) (nth 8 lst))

(defun tenth (lst) (nth 9 lst))

(defun rest (lst) (cdr lst))

(defun nthcdr (n lst)
  (labels ((ithcdr (i lst)
             (if (= i n)
                 lst
                 (ithcdr (+ i 1) (cdr lst)))))
    (ithcdr 0 lst)))

(defun last (lst)
  (labels ((find-last (first rest)
             (if (eq rest nil)
                 first
                 (find-last (car rest) (cdr rest)))))
    (find-last nil lst)))


;;; Exports symbols.

(export '(list-length nth first second third fourth fifth sixth seventh eighth
          ninth tenth rest nthcdr last))


;;; Inherits symbols from common-lisp.

(use-package 'common-lisp 'common-lisp-user)
