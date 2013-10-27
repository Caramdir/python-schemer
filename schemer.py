"""A simple implementation of the scheme version described in "The Little Schemer".

Currently only the first chapter is implemented.

This is a very inefficient implementation. In particular, there is a lot of
copying of lists that could be avoided with better data structures.
"""

def tokenize(code):
    """Split the code into tokens."""
    return code.replace("(", " ( ").replace(")", " ) ").split()

def parse(tokens):
    """Make a list of lists out of the token list.

    We do not check whether all parenthesis are correctly matched. Unmatched
    parenthesis can lead to strange outcomes.
    """
    return _parse_list(iter(tokens))

def _parse_list(tokens_iter):
    l = []
    for token in tokens_iter:
        if token == "(":
            l.append(_parse_list(tokens_iter))
        elif token == ")":
            return l
        else:
            try:
                l.append(int(token))
            except ValueError:
                l.append(token)

    return l

def eval(prog):
    """Recursively evaluate a parsed program."""
    if isinstance(prog, list):
        if not prog:
            return prog
        if isinstance(prog[0], str) and prog[0] in funcs:
            return funcs[prog[0]](*eval(prog[1:]))
        else:
            return [eval(u) for u in prog]
    return prog

def run(code):
    """Run some scheme code."""
    return eval(parse(tokenize(code)))[0]

# Primitive functions

def car(l):
    assert isinstance(l, list) and len(l) > 0
    return l[0]

def cdr(l):
    assert isinstance(l, list) and len(l) > 0
    return l[1:]

def cons(a, l):
    assert isinstance(l, list)
    l.insert(0,a)
    return l

def is_null(l):
    assert isinstance(l, list)
    return not bool(l)

def is_eq(a1, a2):
    assert isinstance(a1, str) and isinstance(a2, str)
    return a1 == a2

funcs = {
        "car": car,
        "cdr": cdr,
        "cons": cons,
        "null?": is_null,
        "atom?": lambda a: not isinstance(a, list),
        "eq?": is_eq,
        }

# Test

prog = """
    (eq? 
        (car (beans beans we need jelly beans))
        (car (cdr (beans beans we need jelly beans)))
    )
"""

print(run(prog))
