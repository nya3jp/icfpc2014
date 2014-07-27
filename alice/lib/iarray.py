def make_array(n, value):
  depth = 0
  width = 1
  while width < n:
    depth += 1
    width *= 2
  return (depth, _make_tree(depth, value))

@nolocals
def _make_tree(depth, value):
  if depth == 0:
    return value
  return (_make_tree(depth - 1, value), _make_tree(depth - 1, value))

def get_array(array, index):
  depth = car(array)
  tree = cdr(array)
  if depth:
    left_size = 1
    for i in xrange(depth - 1):
      left_size *= 2
  else:
    left_size = 0
  for i in xrange(depth):
    if index >= left_size:
      tree = cdr(tree)
      index -= left_size
    else:
      tree = car(tree)
    left_size /= 2
  return tree

def set_array(array, index, value):
  depth = car(array)
  tree = cdr(array)
  left_size = 1
  if depth:
    left_size = 1
    for i in xrange(depth - 1):
      left_size *= 2
  else:
    left_size = 0
  return (depth, _set_tree_impl(tree, index, value, left_size))

@nolocals
def _set_tree_impl(tree, index, value, left_size):
  if left_size == 0:
    return value
  if index >= left_size:
    return (car(tree), _set_tree_impl(cdr(tree), index - left_size, value, left_size / 2))
  return (_set_tree_impl(car(tree), index, value, left_size / 2), cdr(tree))
