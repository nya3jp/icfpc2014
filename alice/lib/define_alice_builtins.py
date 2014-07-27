import __builtin__

__builtin__.car = lambda (a, b): a
__builtin__.cdr = lambda (a, b): b
__builtin__.atom = lambda x: isinstance(x, int)
__builtin__.rank = lambda n: lambda f: f
