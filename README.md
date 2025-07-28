# CLisPy

## Overview
Common Lisp interpreter written in Python (CLisPy) inspired from [Peter Norvig](http://norvig.com/), [lispy](http://norvig.com/lispy.html) and [lispy.py](http://norvig.com/lispy2.html). This interpreter will be implemented to satisfy　ANSI Common Lisp (Syntax, Macro System, CLOS etc).

## Motivation
Common Lisp s-expression have good compatibility with some deep learning frameworks to stack layers. And to build the machine learning model by using scikit-learn, the s-expression also have compatibility for data and model pipeline.

## Required libraries
- Python 3.10.13
- numpy 2.2.5

## Examples
```
$ python
Python 3.10.13 (main, Sep 11 2023, 08:39:02) [Clang 14.0.6 ] on darwin
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

Clispy supports python built-in Functions.
- https://docs.python.org/3/library/functions.html

```
CL-USER=> (py:abs -100)
100

CL-USER=> (py:sorted '(9 8 7 6 5 4 3 2 1 0))
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

CL-USER=> (type-of (py:sorted '(9 8 7 6 5 4 3 2 1 0)))
PYTHON-OBJECT

CL-USER=> (coerce (py:sorted '(9 8 7 6 5 4 3 2 1 0)) 'cons)
(0 1 2 3 4 5 6 7 8 9)

CL-USER=>
```

### Tensorflow bridge

There are python object manipulation functions.
- py:import Imports python modules
- py:attr Gets an attribute of python object.
- py:call Calls a method of python object.
- py:item Gets an item of python sequence object, this function is run with index or py:slice.

```
CL-USER=> (py:import tf "tensorflow"
                     keras "tensorflow.keras")
KERAS

CL-USER=> (setq mnist (py:call keras "datasets.mnist.load_data"))
...

CL-USER=> (setq x-train (py:item (py:item mnist 0) 0)
                y-train (py:item (py:item mnist 0) 1)
                x-test (py:item (py:item mnist 1) 0)
                y-test (py:item (py:item mnist 1) 1))
...

CL-USER=> (setq x-train (/ x-train 255.0)
                x-test (/ x-test 255.0))
...

CL-USER=> (setq model (py:call keras "models.Sequential"
                        (list (py:call keras "layers.Flatten" :input-shape '(28 28))
                              (py:call keras "layers.Dense" 512 :activation (py:attr tf "nn.relu"))
                              (py:call keras "layers.Dropout" 0.2)
                              (py:call keras "layers.Dense" 10 :activation (py:attr tf "nn.softmax")))))
<tensorflow.python.keras.engine.sequential.Sequential object at 0x000001E70CFEF470>

CL-USER=> (py:call model "compile" :optimizer "adam"
                                   :loss "sparse_categorical_crossentropy"
                                   :metrics '("accuracy"))
None

CL-USER=> (py:call model "fit" x-train y-train :epochs 5)
Epoch 1/5
60000/60000 [==============================] - 11s 189us/sample - loss: 0.2195 - acc: 0.9353
Epoch 2/5
60000/60000 [==============================] - 11s 184us/sample - loss: 0.0972 - acc: 0.9704
Epoch 3/5
60000/60000 [==============================] - 11s 184us/sample - loss: 0.0689 - acc: 0.9785
Epoch 4/5
60000/60000 [==============================] - 11s 187us/sample - loss: 0.0534 - acc: 0.9827
Epoch 5/5
60000/60000 [==============================] - 11s 187us/sample - loss: 0.0403 - acc: 0.9871
<tensorflow.python.keras.callbacks.History object at 0x000001E71E9687B8>

CL-USER=> (py:call model "evaluate" x-test y-test)
10000/10000 [==============================] - 1s 56us/sample - loss: 0.0675 - acc: 0.9792
[0.06751626054281369, 0.9792]
```

## References
- [lispy](http://norvig.com/lispy.html)
- [lispy.py](http://norvig.com/lispy2.html)
- [Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm)
- https://github.com/sbcl/sbcl
- https://github.com/armedbear/abcl
- https://docs.python.org/3/library/functions.html
- https://www.tensorflow.org/

## License
[Apache License 2.0](https://github.com/takahish/clispy/blob/master/LICENSE)

## Acknowledgements
I wish to thank [Peter Norvig](http://norvig.com/), [lispy](http://norvig.com/lispy.html) and [lispy.py](http://norvig.com/lispy2.html). I also wish to thank sbcl and abcl for referencing how do they implement common lisp interpreter.
