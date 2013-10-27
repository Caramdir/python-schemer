"""A simple implementation of the scheme version described in "The Little Schemer".

Currently only the first two chapters are implemented.

This is a very inefficient implementation. In particular, there is a lot of
copying of lists that could be avoided with better data structures.
"""

from collections import ChainMap


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
        elif token == "#t":
            l.append(True)
        elif token == "#f":
            l.append(False)
        else:
            try:
                l.append(int(token))
            except ValueError:
                l.append(token)

    return l

def eval(expr, is_question=False):
    """Recursively evaluate a parsed program."""
    #print(expr)

    # atoms
    if not isinstance(expr, list):
        if is_question and expr == "else":
            return True
        if expr in context:
            return context[expr]
        return expr

    # empty list
    if not expr:
        return expr
    
    # buitlt-ins
    if expr[0] == "quote":
        return expr[1]
    if expr[0] == "cond":
        for clause in expr[1:]:
            if eval(clause[0], is_question=True) is True:
                return eval(clause[1])
        return None     # undefined behaviour if nothing matches
    if is_question and expr[0] == "or":
        for test in expr[1:]:
            if eval(test) is True:
                return True
        return False
    if expr[0] == "define":
        context[expr[1]] = eval(expr[2])
        return None
    if expr[0] == "lambda":
        def l(*params):
            global context
            context = context.new_child()
            context.update(zip(expr[1], params))
            r = eval(expr[2])
            context = context.parents
            return r
        return l

    # otherwise recursively evaluate the list
    expr = [eval(u) for u in expr]

    # evaluate functions
    if hasattr(expr[0], '__call__'):
        return expr[0](*eval(expr[1:]))
    else:
        return expr

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
    assert isinstance(l, list), "Argument to null? has to be a list, but is {}.".format(l)
    return not bool(l)

def is_eq(a1, a2):
    assert isinstance(a1, str) and isinstance(a2, str)
    return a1 == a2

# The current execution context.

context = ChainMap({
        "car": car,
        "cdr": cdr,
        "cons": cons,
        "null?": is_null,
        "atom?": lambda a: not isinstance(a, list),
        "eq?": is_eq,
})

# Some basic functions defined in "The Little Schemer".

run("""(
    (define lat?
        (lambda (l)
            (cond
                ((null? l) #t)
                ((atom? (car l)) (lat? (cdr l)))
                (else #f)
            )
        )
    )
    (define member?
        (lambda (a lat)
            (cond
                ((null? lat) #f)
                (else (or (eq? (car lat) a)
                    (member? a (cdr lat))))
            )
        )
    )
    (define rember
        (lambda (a lat)
            (cond
                ((null? lat) (quote ()))
                ((eq? (car lat) a) (cdr lat))
                (else (cons (car lat) (rember a (cdr lat))))
            )
        )
    )
    (define firsts
        (lambda (l)
            (cond
                ((null? l) ())
                (else (cons (car (car l)) (firsts (cdr l))))
            )
        )
    )
    (define insertR
        (lambda (new old lat)
            (cond
                ((null? lat) (quote ()))
                ((eq? old (car lat)) (cons old (cons new (cdr lat))))
                (else (cons (car lat) (insertR new old (cdr lat))))
            )
        )
    )
    (define insertL
        (lambda (new old lat)
            (cond
                ((null? lat) (quote ()))
                ((eq? old (car lat)) (cons new lat))
                (else (cons (car lat) (insertL new old (cdr lat))))
            )
        )
    )
    (define subst
        (lambda (new old lat)
            (cond
                ((null? lat) (quote ()))
                ((eq? old (car lat)) (cons new (cdr lat)))
                (else (cons (car lat) (subst new old (cdr lat))))
            )
        )
    )
    (define subst2
        (lambda (new o1 o2 lat)
            (cond
                ((null? lat) (quote ()))
                ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
                (else (cons (car lat) (subst2 new o1 o2 (cdr lat))))
            )
        )
    )
    (define multirember
        (lambda (a lat)
            (cond
                ((null? lat) (quote ()))
                ((eq? (car lat) a) (multirember a (cdr lat)))
                (else (cons (car lat) (multirember a (cdr lat))))
            )
        )
    )
    (define multiinsertR
        (lambda (new old lat)
            (cond
                ((null? lat) (quote ()))
                ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
                (else (cons (car lat) (multiinsertR new old (cdr lat))))
            )
        )
    )
    (define multiinsertL
        (lambda (new old lat)
            (cond
                ((null? lat) (quote ()))
                ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
                (else (cons (car lat) (multiinsertL new old (cdr lat))))
            )
        )
    )
)""")

# Testing

prog = """
    (multiinsertL fried fish (chips and fish or fish and fried))
"""
print(run(prog))
