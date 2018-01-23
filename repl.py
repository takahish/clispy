import sys
from clispy.parser import _eof_object, _InPort, _parse, _to_string
from clispy.eval import _eval

def repl(prompt='clispy> ', inport=_InPort(sys.stdin), out=sys.stdout):
    """A prompt-read-eval-print loop."
    """
    sys.stderr.write("CLisPy Version 0.1\n")
    sys.stderr.flush() # flush buffer explicitly
    while True:
        try:
            if prompt:
                sys.stderr.write(prompt)
                sys.stderr.flush() # flush buffer explicitly
            x = _parse(inport)
            if x is _eof_object:
                print(file=out)
                return
            val = _eval(x)
            if val is not None and out:
                print(_to_string(val), file=out)
        except Exception as e:
            print('%s: %s' % (type(e).__name__, e))
