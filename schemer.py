"""A simple implementation of the scheme version described in "The Little Schemer".

Currently only the first four chapters are implemented.

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

def eval(expr):
    """Recursively evaluate a parsed program."""
#    print(expr)

    # atoms
    if not isinstance(expr, list):
        if expr == "else":
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
            if eval(clause[0]) is True:
                return eval(clause[1])
        return None     # undefined behaviour if nothing matches
    if expr[0] == "or":
        return any(eval(test) for test in expr[1:])
    if expr[0] == "and":
        return all(eval(test) for test in expr[1:])
    if expr[0] == "define":
        context[expr[1]] = eval(expr[2])
        return None
    if expr[0] == "lambda":
        def l(*params):
            global context
            context = context.new_child()
            context.update(zip(expr[1], params))
            r = eval(expr[2])
            if isinstance(r, list):
                r = r[:]
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
    assert isinstance(l, list) and len(l) > 0, "Argument to 'car' should be a non-empty list, but got {}.".format(l)
    return l[0]

def cdr(l):
    assert isinstance(l, list) and len(l) > 0, "Argument to 'cdr' should be a non-empty list, but got {}.".format(l)
    return l[1:]

def cons(a, l):
    assert isinstance(l, list), "Second argument of 'cons' should be a list, got {}.".format(l)
    l.insert(0,a)
    return l

def is_null(l):
    assert isinstance(l, list), "Argument to 'null?' has to be a list, but is {}.".format(l)
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
        "add1": lambda n: n + 1,
        "sub1": lambda n: n - 1,
        "zero?": lambda n: n is 0,
        "number?": lambda n: isinstance(n, int),
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
    (define multisubst
        (lambda (new old lat)
            (cond
                ((null? lat) (quote ()))
                ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
                (else (cons (car lat) (multisubst new old (cdr lat))))
            )
        )
    )
    (define +
        (lambda (n m)
            (cond
                ((zero? m) n)
                (else (+ (add1 n) (sub1 m)))
            )
        )
    )
    (define -
        (lambda (n m)
            (cond
                ((zero? m) n)
                (else (- (sub1 n) (sub1 m)))
            )
        )
    )
    (define addtup
        (lambda (tup)
            (cond
                ((null? tup) 0)
                (else (+ (car tup) (addtup (cdr tup))))
            )
        )
    )
    (define × 
        (lambda (n m)
            (cond
                ((zero? m) 0)
                (else (+ n (× n (sub1 m))))
            )
        )
    )
    (define * ×)
    (define tup+
        (lambda (tup1 tup2)
            (cond
                ((null? tup1) tup2)
                ((null? tup2) tup1)
                (else 
                    (cons 
                        (+ (car tup1) (car tup2))
                        (tup+ (cdr tup1) (cdr tup2))
                    )
                )
            )
        )
    )
    (define >
        (lambda (n m)
            (cond
                ((zero? n) #f)
                ((zero? m) #t)
                (else (> (sub1 n) (sub1 m)))
            )
        )
    )
    (define <
        (lambda (n m)
            (> m n)
        )
    )
    (define =
        (lambda (n m)
            (cond
                ((or (> n m) (< n m)) #f)
                (else #t)
            )
        )
    )
    (define ↑
        (lambda (n m)
            (cond
                ((zero? m) 1)
                (else (× n (↑ n (sub1 m))))
            )
        )
    )
    (define expt ↑)
    (define ÷
        (lambda (n m)
            (cond
                ((< n m) 0)
                (else (add1 (÷ (- n m) m)))
            )
        )
    )
    (define quotient ÷)
    (define length
        (lambda (lat)
            (cond
                ((null? lat) 0)
                (else (add1 (length (cdr lat))))
            )
        )
    )
    (define pick
        (lambda (n lat)
            (cond
                ((zero? (sub1 n)) (car lat))
                (else (pick (sub1 n) (cdr lat)))
            )
        )
    )
    (define rempick
        (lambda (n lat)
            (cond
                ((one? n) (cdr lat))
                (else (cons (car lat) (rempick (sub1 n) (cdr lat))))
            )
        )
    )
    (define no-nums
        (lambda (lat)
            (cond
                ((null? lat) (quote ()))
                ((number? (car lat)) (no-nums (cdr lat)))
                (else (cons (car lat) (no-nums (cdr lat))))
            )
        )
    )
    (define all-nums
        (lambda (lat)
            (cond
                ((null? lat) (quote ()))
                ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
                (else (all-nums (cdr lat)))
            )
        )
    )
    (define eqan?
        (lambda (a1 a2)
            (cond
                ((and (number? a1) (number? a2)) (= a1 a2))
                ((or (number? a1) (number? a2)) #f)
                (else (eq? a1 a2))
            )
        )
    )
    (define occur
        (lambda (a lat)
            (cond
                ((null? lat) 0)
                ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
                (else (occur a (cdr lat)))
            )
        )
    )
    (define one?
        (lambda  (n)
            (= n 1)
        )
    )
    (define rember*
        (lambda (a l)
            (cond
                ((null? l) (quote ()))
                ((atom? (car l))
                    (cond
                        ((eqan? (car l) a) (rember* a (cdr l)))
                        (else (cons (car l) (rember* a (cdr l))))
                    ))
                (else
                    (cons (rember* a (car l)) (rember* a (cdr l)))
                )
            )
        )
    )
    (define insertR*
        (lambda (new old l)
            (cond
                ((null? l) (quote ()))
                ((atom? (car l))
                    (cond
                        ((eqan? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
                        (else (cons (car l) (insertR* new old (cdr l))))
                    ))
                (else (cons (insertR* new old (car l)) (insertR* new old (cdr l))))
            )
        )
    )
    (define occur*
        (lambda (a l)
            (cond
                ((null? l) 0)
                ((atom? (car l))
                    (cond
                        ((eqan? a (car l)) (add1 (occur* a (cdr l))))
                        (else (occur* a (cdr l)))
                    ))
                (else (+ (occur* a (car l)) (occur* a (cdr l))))
            )
        )
    )
    (define subst*
        (lambda (new old l)
            (cond
                ((null? l) (quote ()))
                ((atom? (car l))
                    (cond
                        ((eqan? old (car l)) (cons new (subst* new old (cdr l))))
                        (else (cons (car l) (subst* new old (cdr l))))
                    ))
                (else (cons (subst* new old (car l)) (subst* new old (cdr l))))
            )
        )
    )
    (define member*
        (lambda (a l)
            (cond
                ((null? l) #f)
                ((atom? (car l)) (or (eqan? a (car l)) (member* a (cdr l))))
                (else (or (member* a (car l)) (member* a (cdr l))))
            )
        )
    )
    (define leftmost
        (lambda (l)
            (cond
                ((atom? (car l)) (car l))
                (else (leftmost (car l)))
            )
        )
    )
    (define eqlist?
        (lambda (l1 l2) 
            (cond
                ((and (null? l1) (null? l2)) #t)
                ((or (null? l1) (null? l2)) #f)
                (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
            )
        )
    )
    (define equal?
        (lambda (s1 s2)
            (cond
                ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
                ((or (atom? s1) (atom? s2)) #f)
                (else (eqlist? s1 s2))
            )
        )
    )
    (define rember
        (lambda (s l)
            (cond
                ((null? l) (quote ()))
                ((equal? (car l) s) (cdr l))
                (else (cons (car l) (rember s (cdr l))))
            )
        )
    )
)""")
# skipped: insertL*, replacing = and eq? by equal?

# Testing

prog = """
"""
print(run(prog))
