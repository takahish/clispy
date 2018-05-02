;;;; Copyright 2018 Takahiro Ishikawa. All Rights Reserved.
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

;; Redefined as macro.
(defmacro lambda (lambda-list &rest body)
  `(defun ,lambda-list ,@body))

;; Redefined as macro.
(defmacro defun (name lambda-list &rest body)
  `(defun ,name (lambda ,lambda-list (block ,name ,@body))))

;; Redefined as macro.
(defmacro defmacro (name lambda-list &rest body)
  `(defmacro ,name (lambda ,lambda-list ,@body)))
