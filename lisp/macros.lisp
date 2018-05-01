(defmacro when (test-form &rest body)
  `(if ,test-form
       (progn ,@body)))

(defmacro unless (test-form &rest body)
  `(if ,(not test-form)
       (progn ,@body)))
