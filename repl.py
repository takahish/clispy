import sys
import symbol
import parser
import eval

def repl(prompt='clispy> ', inport=parser._InPort(sys.stdin), out=sys.stdout):
    """A prompt-read-eval-print loop."
    """
    sys.stderr.write("CLisPy Version 0.2\n")
    sys.stderr.flush() # flush buffer explicitly
    while True:
        try:
            if prompt:
                sys.stderr.write(prompt)
                sys.stderr.flush() # flush buffer explicitly
            x = parser._parse(inport)
            if x is symbol._eof_object:
                print(file=out)
                return
            val = eval._eval(x)
            if val is not None and out:
                print(parser._to_string(val), file=out)
        except Exception as e:
            print('%s: %s' % (type(e).__name__, e))
