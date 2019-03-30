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

(defmacro when (test-form &rest body)
  `(if ,test-form
       (progn ,@body)))

(defmacro unless (test-form &rest body)
  `(if (not ,test-form)
       (progn ,@body)))


;;; Exports symbols.

(export '(when unless))


;;; Inherits symbols from common-lisp.

(use-package 'common-lisp 'common-lisp-user)
