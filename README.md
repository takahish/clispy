# LisPy
Lisp interpreter written in python originally by [Peter Norvig](http://norvig.com/). This interpreter will be custermized to aim conforming R7RS-small (along with learning behavior of interpreter).

## Examples
```
>>> import sys
>>> sys.version_info
sys.version_info(major=3, minor=6, micro=2, releaselevel='final', serial=0)

>>> from lispy.repl import repl
>>> repl()
lispy> pi
3.141592653589793
lispy> (cos pi)
-1.0
lispy> (+ (cos pi) 1)
0.0
lispy> (define a 10)
10
lispy> a
10
lispy> (define func (lambda (x) (* x x)))
<lispy.repl.Procedure object at 0x110194630>
lispy> (func a)
100
```

## Test
```
$ ls -F
... lispy/ ...

$ python -m unittest lispy/test/test_type.py 
$ python -m unittest lispy/test/test_env.py
$ python -m unittest lispy/test/test_parser.py
$ python -m unittest lispy/test/test_repl.py
```

## References
- [lispy](http://norvig.com/lispy.html)
- [lispy.py](http://norvig.com/lispy2.html)
- [R7RS](https://bitbucket.org/cowan/r7rs-wg1-infra/src/default/R7RSHomePage.md?fileviewer=file-view-default)
