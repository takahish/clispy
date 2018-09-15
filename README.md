# CLisPy

## Overview
Common Lisp interpreter written in Python (CLisPy) inspired from [Peter Norvig](http://norvig.com/), [lispy](http://norvig.com/lispy.html) and [lispy.py](http://norvig.com/lispy2.html). This interpreter will be implemented to satisfy　ANSI Common Lisp (Syntax, Macro System, CLOS etc).

## Motivation
Comon lisp s-expression have good compatibility with some deep learning frameworks to stack layers. And to build the machine learning model by using scikit-learn, the s-expression also have compatibility for data and model pipline.

## Required libraries
- Python 3.x
- numpy 1.14+

## Examples
```
>>> import clispy
>>> clispy.repl()
CLisPy Version 0.2
clispy> (setq pi 3.14)
3.14
clispy> (defun area (r) (* pi r r))
AREA
clispy> (area 3)
28.259999999999998
clispy> (defun fact (n)
          (if (<= n 1)
              1
              (* n (fact (- n 1)))))
FACT
clispy> (fact 10)
3628800
clispy> (fact 50)
30414093201713378043612608166064768844377641568960512000000000000
clispy> (defmacro when (test-form &rest body)
          `(if ,test-form
             (progn ,@body)))
WHEN
clispy> (When t 3)
3
clispy> (when nil 3)
NIL
clispy> (car '(1 2 3 4 5))
1
clispy> (cdr '(1 2 3 4 5))
(2 3 4 5)
clispy> (cons 'a nil)
(A)
clispy> (cons 'a 'b)
(A . B)
clispy> (car '(a . b))
A
clispy> (cdr '(a . b))
B
clispy> ^D
>>> 
```

## References
- [lispy](http://norvig.com/lispy.html)
- [lispy.py](http://norvig.com/lispy2.html)
- [Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm)
- https://github.com/sbcl/sbcl
- https://github.com/armedbear/abcl

## License
[Apache License 2.0](https://github.com/takahish/clispy/blob/master/LICENSE)

## Acknowledgements
I wish to thank [Peter Norvig](http://norvig.com/), [lispy](http://norvig.com/lispy.html) and [lispy.py](http://norvig.com/lispy2.html). I also wish to thank sbcl and abcl for referencing how do they implement common lisp interpreter.
