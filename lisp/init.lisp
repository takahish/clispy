;; Redefined by using defmacro.
(defmacro lambda (lambda-list &rest body)
  `(defun ,lambda-list ,@body))

;; Redefined by using defmacro.
(defmacro defun (name lambda-list &rest body)
  `(defun ,name (lambda ,lambda-list (block ,name ,@body))))

;; Redefined by using defmacro.
(defmacro defmacro (name lambda-list &rest body)
  `(defmacro ,name (lambda ,lambda-list ,@body)))
