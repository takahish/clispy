# CLisPy
Lisp interpreter written in python originally by [Peter Norvig](http://norvig.com/), [lispy](http://norvig.com/lispy.html) and [lispy.py](http://norvig.com/lispy2.html). CLisPy means Comfortable Lisp interpreter written in Python. This interpreter will be custermized to aim cherry picking R7RS-small (Basic Syntax etc) or ANSI Common Lisp (Macro System, CLOS etc) (along with learning behavior of interpreter).

## Examples
```
>>> import sys
>>> sys.version_info
sys.version_info(major=3, minor=6, micro=2, releaselevel='final', serial=0)

>>> import sys
>>> sys.version_info
sys.version_info(major=3, minor=6, micro=2, releaselevel='final', serial=0)

>>> from clispy.repl import repl
>>> repl()
CLisPy Version 0.1
clispy> (define area (lambda (r) (* pi (* r r))))
clispy> (area 3)
28.274333882308138
clispy> (define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))
clispy> (fact 10)
3628800
clispy> (fact 100)
93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
clispy> (area (fact 10))
41369087205782.695
clispy> (define first car)
clispy> (define rest cdr)
clispy> (define count (lambda (item L) (if L (+ (equal? item (first L)) (count item (rest L))) 0)))
clispy> (count 0 (list 0 1 2 3 0 0))
3
clispy> (count (quote the) (quote (the more the merrier the bigger the better)))
4
clispy> ^D

>>>
```

## Unit tests
```
$ ls -F
... clispy/ ...

$ python -m unittest
```

## References
- [lispy](http://norvig.com/lispy.html)
- [lispy.py](http://norvig.com/lispy2.html)
- [R7RS](https://bitbucket.org/cowan/r7rs-wg1-infra/src/default/R7RSHomePage.md?fileviewer=file-view-default)
- [Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm)
