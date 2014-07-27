#!/usr/bin/python

import ast
import collections
import re
import sys
import traceback


class CompileError(Exception):
  def __init__(self, msg, line, column):
    Exception.__init__(self, msg)
    self.line = line
    self.column = column


def compile_assert(cond, node, tmpl='UNDOCUMENTED ERROR', *args):
  if not cond:
    if node:
      line = getattr(node, 'lineno', None)
      column = getattr(node, 'col_offset', None)
    else:
      line, column = None, None
    loc = ' at line %d, column %d' % (line, column) if line else ''
    msg = tmpl % args
    raise CompileError(msg + loc, line, column)


class Context(object):
  def __init__(self):
    self.vars = {}
    self.funcs = {}
    self.current_loop = None
    self.lines = []
    self._seq = [0]

  def copy(self):
    ctx = Context()
    ctx.vars = self.vars.copy()
    ctx.funcs = self.funcs.copy()
    ctx.current_loop = self.current_loop
    ctx.lines = self.lines
    ctx._seq = self._seq
    return ctx

  def make_label(self):
    label = 'ephemeral%d' % self._seq[0]
    self._seq[0] += 1
    return label

  def emit(self, tmpl, *args):
    s = tmpl % args
    if s and s[0].isupper():
      s = '  %s' % s
    self.lines.append(s)

  def output(self):
    return '\n'.join(self.lines)


class Syntax(object):
  def compile(self, ctx):
    raise NotImplementedError()


class Stmt(Syntax):
  def __init__(self, children):
    self.children = children

  def gather_func_vars(self):
    vars = []
    for stmt in self.children:
      vars.extend(stmt.gather_func_vars())
    return vars

  def gather_func_ranks(self, ctx):
    ranks = []
    for stmt in self.children:
      ranks.extend(stmt.gather_func_ranks(ctx))
    return ranks

  def compile(self, ctx):
    for stmt in self.children:
      stmt.compile(ctx)


class Block(Stmt):
  pass


class Module(Syntax):
  def __init__(self, funcs):
    self.funcs = funcs

  def compile(self, ctx):
    ctx = ctx.copy()
    compile_assert(
        self.funcs[0].name == 'main',
        None,
        'First function must be named as "main"')
    ctx.funcs = dict((func.name, func) for func in self.funcs)
    for func in self.funcs:
      if func.rank is None:
        ranks = sorted(set(func.gather_func_ranks(ctx)))
        compile_assert(
            len(ranks) <= 1,
            None,
            'A function has multiple candidate ranks: %s',
            ', '.join(str(rank) for rank in ranks))
        func.rank = ranks[0] if ranks else 0
    for func in self.funcs:
      func.compile(ctx)


class Function(Stmt):
  def __init__(self, name, args, block, rank):
    Stmt.__init__(self, block.children)
    self.name = name
    self.args = args
    self.block = block
    self.rank = rank
    self.label = name

  def compile(self, ctx):
    ctx = ctx.copy()
    locals = set(self.gather_func_vars())
    locals -= set(self.args)
    locals = sorted(locals)
    assert not ctx.vars
    # TODO: handle case where there is no locals
    for i, local in enumerate(locals):
      ctx.vars[local] = (0, i)
    for i, arg in enumerate(self.args):
      ctx.vars[arg] = (1, i)
    ctx.emit('%s:', ctx.funcs[self.name].label)
    for local in locals:
      ctx.emit('LDC 0  ; %s', local)
    ctx.emit('LDF %s_body', ctx.funcs[self.name].label)
    ctx.emit('AP %d', len(locals))
    ctx.emit('RTN')
    ctx.emit('%s_body:', ctx.funcs[self.name].label)
    self.block.compile(ctx)
    ctx.emit('RTN')


class If(Stmt):
  def __init__(self, test, then_block, else_block):
    Stmt.__init__(self, then_block.children + else_block.children)
    self.test = test
    self.then_block = then_block
    self.else_block = else_block

  def compile(self, ctx):
    self.test.compile(ctx)
    label = ctx.make_label()
    ctx.emit('TSEL %s_then %s_else', label, label)
    ctx.emit('%s_then:', label)
    self.then_block.compile(ctx)
    ctx.emit('LDC 283283283')
    ctx.emit('TSEL %s_end %s_end', label, label)
    ctx.emit('%s_else:', label)
    self.else_block.compile(ctx)
    ctx.emit('%s_end:', label)


class Return(Stmt):
  def __init__(self, values):
    Stmt.__init__(self, [])
    self.values = values

  def gather_func_ranks(self, ctx):
    return [len(self.values)]

  def compile(self, ctx):
    for value in self.values:
      value.compile(ctx)
    ctx.emit('RTN')


class Assign(Stmt):
  def __init__(self, targets, value):
    Stmt.__init__(self, [])
    self.targets = targets
    self.value = value

  def gather_func_vars(self):
    return list(self.targets)

  def compile(self, ctx):
    self.value.compile(ctx)
    for target in reversed(self.targets):
      a, b = ctx.vars[target]
      ctx.emit('ST %d %d  ; %s', a, b, target)


class Discard(Stmt):
  def __init__(self, value):
    Stmt.__init__(self, [])
    self.value = value

  def gather_func_vars(self):
    return ['_']

  def compile(self, ctx):
    self.value.compile(ctx)
    for i in xrange(self.value.rank(ctx)):
      a, b = ctx.vars['_']
      ctx.emit('ST %d %d  ; discard', a, b)


class While(Stmt):
  def __init__(self, test, block):
    Stmt.__init__(self, block.children)
    self.test = test
    self.block = block

  def compile(self, ctx):
    last_loop = ctx.current_loop
    label = ctx.make_label()
    loop = {'cond': '%s_cond' % label,
            'next': '%s_next' % label,
            'body': '%s_body' % label,
            'exit': '%s_exit' % label}
    ctx.current_loop = loop
    ctx.emit('%s:', loop['cond'])
    ctx.emit('%s:', loop['next'])
    self.test.compile(ctx)
    ctx.emit('TSEL %s %s', loop['body'], loop['exit'])
    ctx.emit('%s:', loop['body'])
    self.block.compile(ctx)
    ctx.emit('LDC 283283283')
    ctx.emit('TSEL %s_cond %s_cond', label, label)
    ctx.emit('%s:', loop['exit'])
    ctx.current_loop = last_loop


class ForN(Stmt):
  def __init__(self, target, begin, end, step, block):
    Stmt.__init__(self, block.children)
    self.target = target
    self.begin = begin
    self.end = end
    self.step = step
    self.block = block

  def gather_func_vars(self):
    return [self.target]

  def compile(self, ctx):
    last_loop = ctx.current_loop
    label = ctx.make_label()
    loop = {'cond': '%s_cond' % label,
            'next': '%s_next' % label,
            'body': '%s_body' % label,
            'exit': '%s_exit' % label}

    ctx.current_loop = loop
    a, b = ctx.vars[self.target]

    self.begin.compile(ctx)
    ctx.emit('ST %d %d  ; %s', a, b, self.target)

    ctx.emit('%s:', loop['cond'])
    self.end.compile(ctx)
    ctx.emit('LD %d %d  ; %s', a, b, self.target)
    ctx.emit('CGT')
    ctx.emit('TSEL %s %s', loop['body'], loop['exit'])

    ctx.emit('%s:', loop['body'])
    self.block.compile(ctx)

    ctx.emit('%s:', loop['next'])
    ctx.emit('LD %d %d  ; %s', a, b, self.target)
    self.step.compile(ctx)
    ctx.emit('ADD')
    ctx.emit('ST %d %d  ; %s', a, b, self.target)
    ctx.emit('LDC 283283283')
    ctx.emit('TSEL %s %s', loop['cond'], loop['cond'])

    ctx.emit('%s:', loop['exit'])

    ctx.current_loop = last_loop


class Continue(Stmt):
  def __init__(self):
    Stmt.__init__(self, [])

  def compile(self, ctx):
    compile_assert(ctx.current_loop, None, 'continue outside loop')
    ctx.emit('LDC 283283283')
    ctx.emit('TSEL %s %s', ctx.current_loop['next'], ctx.current_loop['next'])


class Break(Stmt):
  def __init__(self):
    Stmt.__init__(self, [])

  def compile(self, ctx):
    compile_assert(ctx.current_loop, None, 'break outside loop')
    ctx.emit('LDC 283283283')
    ctx.emit('TSEL %s %s', ctx.current_loop['exit'], ctx.current_loop['exit'])


class Print(Stmt):
  def __init__(self, values):
    Stmt.__init__(self, [])
    self.values = values

  def compile(self, ctx):
    for i, value in enumerate(self.values):
      value.compile(ctx)
      if i > 0:
        ctx.emit('CONS')
    ctx.emit('DBUG')


class Pass(Stmt):
  def __init__(self):
    Stmt.__init__(self, [])

  def compile(self, ctx):
    pass


class Expr(Syntax):
  def rank(self, ctx):
    raise NotImplementedError()


class UnaryOp(Expr):
  def __init__(self, operand):
    self.operand = operand

  def rank(self, ctx):
    compile_assert(self.operand.rank(ctx) == 1, None)
    return 1

  def compile(self, ctx):
    raise NotImplementedError()


class BinOp(Expr):
  def __init__(self, left, right):
    self.left = left
    self.right = right

  def rank(self, ctx):
    compile_assert(self.left.rank(ctx) == 1, None)
    compile_assert(self.right.rank(ctx) == 1, None)
    return 1

  def compile(self, ctx):
    raise NotImplementedError()


class NumBinOp(BinOp):
  def __init__(self, inst, left, right):
    self.inst = inst
    BinOp.__init__(self, left, right)

  def compile(self, ctx):
    if self.inst.startswith('!'):
      ctx.emit('LDC 1')
    self.left.compile(ctx)
    self.right.compile(ctx)
    ctx.emit(self.inst.lstrip('!'))
    if self.inst.startswith('!'):
      ctx.emit('SUB')

  @classmethod
  def op_to_inst(cls, op):
    if isinstance(op, ast.Add):
      return 'ADD'
    if isinstance(op, ast.Sub):
      return 'SUB'
    if isinstance(op, ast.Mult):
      return 'MUL'
    if isinstance(op, ast.Div):
      return 'DIV'
    if isinstance(op, ast.Eq):
      return 'CEQ'
    if isinstance(op, ast.NotEq):
      return '!CEQ'
    if isinstance(op, ast.Gt):
      return 'CGT'
    if isinstance(op, ast.GtE):
      return 'CGTE'
    if isinstance(op, ast.Lt):
      return '!CGTE'
    if isinstance(op, ast.LtE):
      return '!CGT'
    return None


class And(BinOp):
  def compile(self, ctx):
    ctx.emit('LDC 1')
    self.left.compile(ctx)
    ctx.emit('LDC 0')
    ctx.emit('CEQ')
    ctx.emit('SUB')
    ctx.emit('LDC 1')
    self.right.compile(ctx)
    ctx.emit('LDC 0')
    ctx.emit('CEQ')
    ctx.emit('SUB')
    ctx.emit('MUL')


class Or(BinOp):
  def compile(self, ctx):
    ctx.emit('LDC 1')
    self.left.compile(ctx)
    ctx.emit('LDC 0')
    ctx.emit('CEQ')
    self.right.compile(ctx)
    ctx.emit('LDC 0')
    ctx.emit('CEQ')
    ctx.emit('MUL')
    ctx.emit('SUB')


class Not(UnaryOp):
  def compile(self, ctx):
    self.operand.compile(ctx)
    ctx.emit('LDC 0')
    ctx.emit('CEQ')


class Call(Expr):
  def __init__(self, func, args):
    self.func = func
    self.args = args

  def rank(self, ctx):
    compile_assert(
        self.func in ctx.funcs,
        None,
        'Undefined function %s',
        self.func)
    compile_assert(
        ctx.funcs[self.func].rank is not None,
        None,
        'Could not determine function rank for %s. '
        'Please annotate it with @rank(n).',
        self.func)
    return ctx.funcs[self.func].rank

  def compile(self, ctx):
    compile_assert(
        self.func in ctx.funcs,
        None,
        'Undefined function %s',
        self.func)
    for arg in self.args:
      arg.compile(ctx)
    ctx.emit('LDF %s', ctx.funcs[self.func].label)
    ctx.emit('AP %d', len(ctx.funcs[self.func].args))


class Car(Expr):
  def __init__(self, pair):
    self.pair = pair

  def rank(self, ctx):
    return 1

  def compile(self, ctx):
    self.pair.compile(ctx)
    ctx.emit('CAR')


class Cdr(Expr):
  def __init__(self, pair):
    self.pair = pair

  def rank(self, ctx):
    return 1

  def compile(self, ctx):
    self.pair.compile(ctx)
    ctx.emit('CDR')


class Pair(Expr):
  def __init__(self, car, cdr):
    self.car = car
    self.cdr = cdr

  def rank(self, ctx):
    return 1

  def compile(self, ctx):
    self.car.compile(ctx)
    self.cdr.compile(ctx)
    ctx.emit('CONS')


class List(Expr):
  def __init__(self, elems):
    self.elems = elems

  def rank(self, ctx):
    return 1

  def compile(self, ctx):
    for elem in self.elems:
      elem.compile(ctx)
    ctx.emit('LDC 0  ; nil')
    for _ in self.elems:
      ctx.emit('CONS')


class Name(Expr):
  def __init__(self, name):
    self.name = name

  def rank(self, ctx):
    return 1

  def compile(self, ctx):
    if self.name in ctx.funcs:
      ctx.emit('LDF %s', ctx.funcs[self.name].label)
    else:
      a, b = ctx.vars[self.name]
      ctx.emit('LD %d %d  ; %s', a, b, self.name)


class Num(Expr):
  def __init__(self, n):
    self.n = n

  def rank(self, ctx):
    return 1

  def compile(self, ctx):
    ctx.emit('LDC %d', self.n)


class Assembly(Expr):
  def __init__(self, code):
    self.code = code

  def rank(self, ctx):
    return 0

  def compile(self, ctx):
    ctx.emit(code)


def parse_module(module):
  compile_assert(isinstance(module, ast.Module), module)
  return Module([parse_func(func) for func in module.body])


def parse_func(func):
  compile_assert(isinstance(func, ast.FunctionDef), func)
  rank = None
  for deco in func.decorator_list:
    if isinstance(deco, ast.Call) and deco.func.id == 'rank':
      compile_assert(len(deco.args) == 1, deco, 'wrong number of args to @rank')
      compile_assert(isinstance(deco.args[0], ast.Num), deco, '@rank arg must be constant')
      rank = deco.args[0].n
  compile_assert(not func.args.defaults, func)
  compile_assert(not func.args.kwarg, func)
  compile_assert(not func.args.vararg, func)
  return Function(func.name,
                  [arg.id for arg in func.args.args],
                  parse_block(func.body),
                  rank)


def parse_block(block):
  return Block([parse_stmt(stmt) for stmt in block])


def parse_stmt(stmt):
  compile_assert(isinstance(stmt, ast.stmt), stmt)
  if isinstance(stmt, ast.If):
    return If(parse_expr(stmt.test),
              parse_block(stmt.body),
              parse_block(stmt.orelse))
  if isinstance(stmt, ast.Return):
    if stmt.value is None:
      return Return([])
    if isinstance(stmt.value, ast.Tuple):
      return Return([parse_expr(el) for el in stmt.value.elts])
    return Return([parse_expr(stmt.value)])
  if isinstance(stmt, ast.Assign):
    compile_assert(len(stmt.targets) == 1, stmt)
    names = []
    if isinstance(stmt.targets[0], ast.Tuple):
      for el in stmt.targets[0].elts:
        compile_assert(isinstance(el, ast.Name), stmt)
        names.append(el.id)
    else:
      compile_assert(isinstance(stmt.targets[0], ast.Name), stmt)
      names.append(stmt.targets[0].id)
    return Assign(names, parse_expr(stmt.value))
  if isinstance(stmt, ast.AugAssign):
    return Assign([stmt.target.id],
                  parse_expr(ast.BinOp(left=stmt.target, op=stmt.op, right=stmt.value)))
  if isinstance(stmt, ast.Expr):
    return Discard(parse_expr(stmt.value))
  if isinstance(stmt, ast.While):
    compile_assert(not stmt.orelse, stmt, 'while-else clause not supported')
    return While(parse_expr(stmt.test), parse_block(stmt.body))
  if isinstance(stmt, ast.Continue):
    return Continue()
  if isinstance(stmt, ast.Break):
    return Break()
  if isinstance(stmt, ast.For):
    compile_assert(not stmt.orelse, stmt, 'for-else clause not supported')
    # for i in xrange(...):
    if isinstance(stmt.iter, ast.Call) and stmt.iter.func.id in ('range', 'xrange'):
      args = [parse_expr(arg) for arg in stmt.iter.args]
      compile_assert(len(args) in (1, 2, 3), stmt, 'unsupported range args')
      if len(args) == 1:
        args = [Num(0), args[0], Num(1)]
      if len(args) == 2:
        args = [args[0], args[1], Num(1)]
      return ForN(stmt.target.id, args[0], args[1], args[2],
                  parse_block(stmt.body))
    compile_assert(False, stmt, 'Unsupported form of for statement')
  if isinstance(stmt, ast.Print):
    compile_assert(not stmt.dest, stmt, 'print destination not supported')
    return Print([parse_expr(value) for value in stmt.values])
  if isinstance(stmt, ast.Pass):
    return Pass()
  compile_assert(False, stmt, 'Unsupported statement %s', stmt.__class__.__name__)


def parse_expr(expr):
  compile_assert(isinstance(expr, ast.expr), expr)
  if isinstance(expr, ast.BinOp):
    inst = NumBinOp.op_to_inst(expr.op)
    return NumBinOp(inst, parse_expr(expr.left), parse_expr(expr.right))
  if isinstance(expr, ast.BoolOp):
    if isinstance(expr.op, ast.And):
      result = parse_expr(expr.values[0])
      for value in expr.values[1:]:
        result = And(result, parse_expr(value))
      return result
    if isinstance(expr.op, ast.Or):
      result = parse_expr(expr.values[0])
      for value in expr.values[1:]:
        result = Or(result, parse_expr(value))
      return result
    compile_assert(False, expr, 'Unsupported boolean operator')
  if isinstance(expr, ast.UnaryOp):
    if isinstance(expr.op, ast.Not):
      return Not(parse_expr(expr.operand))
    compile_assert(False, expr, 'Unsupported unary operator')
  if isinstance(expr, ast.Compare):
    compile_assert(len(expr.ops) == 1, expr, 'multiple comparison not supported')
    compile_assert(len(expr.comparators) == 1, expr, 'multiple comparison not supported')
    inst = NumBinOp.op_to_inst(expr.ops[0])
    return NumBinOp(inst, parse_expr(expr.left), parse_expr(expr.comparators[0]))
  if isinstance(expr, ast.Call):
    compile_assert(not expr.keywords, expr, 'keywords not supported')
    compile_assert(not expr.kwargs, expr, 'keyword args not supported')
    compile_assert(not expr.starargs, expr, 'star args not supported')
    if expr.func.id == 'car':
      compile_assert(len(expr.args) == 1, expr, 'wrong number of args to car')
      return Car(parse_expr(expr.args[0]))
    if expr.func.id == 'cdr':
      compile_assert(len(expr.args) == 1, expr, 'wrong number of args to cdr')
      return Cdr(parse_expr(expr.args[0]))
    return Call(expr.func.id, [parse_expr(arg) for arg in expr.args])
  if isinstance(expr, ast.Subscript):
    return Call('_builtin_index',
                [parse_expr(expr.value), parse_expr(expr.slice.value)])
  if isinstance(expr, ast.Tuple):
    compile_assert(len(expr.elts) == 2, expr, 'Only 2-tuples are allowed')
    return Pair(parse_expr(expr.elts[0]), parse_expr(expr.elts[1]))
  if isinstance(expr, ast.List):
    return List([parse_expr(el) for el in expr.elts])
  if isinstance(expr, ast.Name):
    return Name(expr.id)
  if isinstance(expr, ast.Num):
    return Num(expr.n)
  #if isinstance(expr, ast.Str):
  #  return Assembly(expr.s)
  compile_assert(False, expr, 'Unsupported expression %s', expr.__class__.__name__)


PRELUDE = """

### PRELUDE ###
@rank(1)
def _builtin_index(array, index):
  while index > 0:
    array = cdr(array)
    index -= 1
  return car(array)

@rank(2)
def unpair(pair):
  return car(pair), cdr(pair)
"""


def resolve_labels(asm):
  lines = asm.splitlines()
  pc = 0
  label_map = collections.OrderedDict()
  ops = []
  for i, line in enumerate(lines):
    s = re.sub(r';.*', '', line).strip()
    if s.endswith(':'):
      label = s.strip(':')
      label_map[label] = pc
    elif s:
      ops.append(s)
      pc += 1
  for i, op in enumerate(ops):
    for label, pc in label_map.iteritems():
      op = re.sub(r'\b%s\b' % label, str(pc), op)
    ops[i] = op
  return '\n'.join(ops)


def main():
  if len(sys.argv) < 2:
    print >>sys.stderr, 'usage: alice.py input.py'
    return
  with open(sys.argv[1]) as f:
    code = f.read()
  code += PRELUDE
  root = ast.parse(code)
  ctx = Context()
  try:
    module = parse_module(root)
    module.compile(ctx)
  except CompileError as e:
    traceback.print_exc()
    if e.line is not None:
      line = code.splitlines()[e.line - 1]
      prefix = '%d:' % e.line
      print >>sys.stderr, prefix + line
      print >>sys.stderr, ' ' * (len(prefix) + e.column) + '^'
  asm = ctx.output()
  asm = resolve_labels(asm)
  print asm


if __name__ == '__main__':
  main()
