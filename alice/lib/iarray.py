def make_array(n, value):
  width = 1
  while width < n:
    width *= 2
  return (width, _make_tree(width, value))

@nolocals
def _make_tree(width, value):
  if width == 1:
    return value
  return (_make_tree(width / 2, value), _make_tree(width / 2, value))

def get_array(array, index):
  left_size = car(array) / 2
  tree = cdr(array)
  while left_size:
    if index >= left_size:
      tree = cdr(tree)
      index -= left_size
    else:
      tree = car(tree)
    left_size /= 2
  return tree

@asm
@rank(1)
def get_array_fast(array, index):
  """
  ; left_size = car(array) / 2
  LD 0 0
  CAR
  LDC 2
  DIV
  ; array = cdr(array)
  LD 0 0
  CDR
  ST 0 0
  LDF %_body
  AP 1
  RTN
%_body:
  ; while left_size:
  LD 0 0
  TSEL %_descent %_exit
%_descent:
  ; if index >= left_size:
  LD 1 1
  LD 0 0
  CGTE
  TSEL %_right %_left
%_right:
  ; array = cdr(array)
  LD 1 0
  CDR
  ST 1 0
  ; index -= left_size
  LD 1 1
  LD 0 0
  SUB
  ST 1 1
  ; left_size /= 2
  LD 0 0
  LDC 2
  DIV
  ST 0 0
  LDC 283283283
  TSEL %_body %_body
%_left:
  ; array = car(array)
  LD 1 0
  CAR
  ST 1 0
  ; left_size /= 2
  LD 0 0
  LDC 2
  DIV
  ST 0 0
  LDC 283283283
  TSEL %_body %_body
%_exit:
  ; return array
  LD 1 0
  RTN
  """

def set_array(array, index, value):
  width = car(array)
  left_size = width / 2
  tree = cdr(array)
  return (width, _set_tree_impl(tree, index, value, left_size))

@nolocals
def _set_tree_impl(tree, index, value, left_size):
  if left_size == 0:
    return value
  if index >= left_size:
    return (car(tree), _set_tree_impl(cdr(tree), index - left_size, value, left_size / 2))
  return (_set_tree_impl(car(tree), index, value, left_size / 2), cdr(tree))
