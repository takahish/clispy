# CLisPy
Common Lisp interpreter written in Python (CLisPy) inspired from [Peter Norvig](http://norvig.com/), [lispy](http://norvig.com/lispy.html) and [lispy.py](http://norvig.com/lispy2.html). This interpreter will be implemented to aim cherry picking ANSI Common Lisp (Syntax, Macro System, CLOS etc).

## Examples
```
$ pwd
... /clispy ...

$ python

>>> import sys
>>> sys.version_info
sys.version_info(major=3, minor=6, micro=2, releaselevel='final', serial=0)

>>> import repl
>>> repl.repl()
CLisPy Version 0.2
clispy> (setq pi 3.14)
3.14

clispy> (defun area (r) (* pi r r))
<eval._Procedure object at 0x10bd78c18>
clispy> (area 3)
28.259999999999998

clispy> (defun fact (n)
          (if (<= n 1)
              1
              (* n (fact (- n 1)))))
<eval._Procedure object at 0x10bd78be0>
clispy> (fact 10)
3628800
clispy> (fact 50)
30414093201713378043612608166064768844377641568960512000000000000
clispy> (fact 100)
93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000clispy> (car (list 1 2 3 4 5))

clispy> (car (list 1 2 3 4 5))
1
clispy> (cdr (list 1 2 3 4 5))
(2 3 4 5)

clispy> (setq first #'car)
<function _car at 0x10bd5e268>
clispy> (setq rest #'cdr)
<function _cdr at 0x10bd5e2f0>
clispy> (funcall first (list 1 2 3 4 5))
1
clispy> (funcall rest (list 1 2 3 4 5))
(2 3 4 5)

clispy> (defmacro when (test &rest body)
          `(if ,test
               (progn ,@body)))
<eval._Procedure object at 0x108f7bb70>
clispy> (when (= 2 3) 4)
NIL
clispy> (when (= 2 2) 4)
4

clispy> (cons 'a 'b))
(A . B)
clispy> (car '(a . b))
A
clispy> (cdr '(a . b))
B

clispy> ^D

>>>
```

## Unit tests
```
$ pwd
... /clispy ...

$ ./unittest.sh
```

## References
- [lispy](http://norvig.com/lispy.html)
- [lispy.py](http://norvig.com/lispy2.html)
- [Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm)
