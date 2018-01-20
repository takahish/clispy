# LisPy
Lisp interpreter written in python originally by [Peter Norvig](http://norvig.com/). This interpreter will be custermized to aim conforming R7RS-small (along with learning behavior of interpreter).

## Examples
```
>>> import sys
>>> sys.version_info
sys.version_info(major=3, minor=6, micro=2, releaselevel='final', serial=0)

>>> from lispy.repl import repl
>>> repl()
lispy> (define area (lambda (r) (* pi (* r r))))
<lispy.eval.Procedure object at 0x10860da20>
lispy> (area 3)
28.274333882308138
lispy> (define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))
<lispy.eval.Procedure object at 0x108612208>
lispy> (fact 10)
3628800
lispy> (fact 100)
93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
lispy> (area (fact 10))
41369087205782.695
lispy> (define first car)
<function standard_env.<locals>.<lambda> at 0x10861c620>
lispy> (define rest cdr)
<function standard_env.<locals>.<lambda> at 0x10861c6a8>
lispy> (define count (lambda (item L) (if L (+ (equal? item (first L)) (count item (rest L))) 0)))
<lispy.eval.Procedure object at 0x1086217f0>
lispy> (count 0 (list 0 1 2 3 0 0))
3
lispy> (count (quote the) (quote (the more the merrier the bigger the better)))
4
```

## Unit tests
```
$ ls -F
... lispy/ ...

$ python -m unittest
```

## References
- [lispy](http://norvig.com/lispy.html)
- [lispy.py](http://norvig.com/lispy2.html)
- [R7RS](https://bitbucket.org/cowan/r7rs-wg1-infra/src/default/R7RSHomePage.md?fileviewer=file-view-default)
