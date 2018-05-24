# CLisPy
Common Lisp interpreter written in Python (CLisPy) inspired from [Peter Norvig](http://norvig.com/), [lispy](http://norvig.com/lispy.html) and [lispy.py](http://norvig.com/lispy2.html). This interpreter will be implemented to aim cherry picking ANSI Common Lisp (Syntax, Macro System, CLOS etc).

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

## License
[Apache License 2.0](https://github.com/takahish/clispy/blob/master/LICENSE)
