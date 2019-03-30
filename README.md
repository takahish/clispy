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
$ python
Python 3.7.1 (default, Dec 14 2018, 19:28:38) 
[GCC 7.3.0] :: Anaconda, Inc. on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> import clispy; clispy.repl()
CL-USER=> (setq pi 3.14)
3.14

CL-USER=> (defun area (r) (* pi r r))
AREA

CL-USER=> (area 3)
28.26

CL-USER=> (defmacro when (test-form &rest body)
            `(if ,test-form
               (progn ,@body)))
WHEN

CL-USER=> (when t 3)
3

CL-USER=> (when nil 3)
NIL

CL-USER=> (car '(1 2 3 4 5))
1

CL-USER=> (cdr '(1 2 3 4 5))
(2 3 4 5)

CL-USER=> (cons 'a nil)
(A)

CL-USER=> (cons 'a 'b)
(A . B)

CL-USER=> (quit)
Although never is often better than *right* now.

>>> 
```

### Python bridge
```
CL-USER=> (python:abs -100)
100

CL-USER=> (python:sorted '(9 8 7 6 5 4 3 2 1 0))
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

CL-USER=> (type-of (python:sorted '(9 8 7 6 5 4 3 2 1 0)))
PYTHON-OBJECT

CL-USER=> (coerce (python:sorted '(9 8 7 6 5 4 3 2 1 0)) 'cons)
(0 1 2 3 4 5 6 7 8 9)

CL-USER=>
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
