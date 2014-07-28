###
### How is the progress?
###

def main(world, ghosts):
  original_map = get_map(world)
  map_size = len(original_map) * len(original_map[0])
  if map_size < 400:
    return 0
  map = convert_2d_array(get_map(world))
  return (map, step)

def step(map, world):
  x, y = get_my_pos(world)
  map = set_2d_array(map, y, x, 1)
  dir = decide(map, world)
  print dir
  return (map, dir)

def decide(map, world):
  cx, cy = get_my_pos(world)
  cdir = get_my_dir(world)
  best_dir = 0
  best_score = -1000000
  for dir in xrange(4):
    score = eval_move(adj_move(cx, cy, dir), dir, 0, 1, map, world)
    if dir == (cdir + 2) % 4:
      score -= 1
    print dir, score
    if score > best_score:
      best_dir = dir
      best_score = score
  return best_dir

def eval_move(cx, cy, cdir, branches, dist, map, world):
  cell_score = eval_cell(cx, cy, dist, map, world)
  if cell_score < 0:
    return cell_score
  future_score = -1000000
  choices = []
  for ndir in range(4):
    if ndir == (cdir + 2) % 4:
      continue
    if get_cell(adj_move(cx, cy, ndir), map) > 0:
      choices = (ndir, choices)  # prepend
  if len(choices) >= 2:
    branches += 1
    if branches >= 3:
      return cell_score
  for ndir in choices:
    nscore = eval_move(adj_move(cx, cy, ndir), ndir, branches, dist + 1, map, world)
    if nscore > future_score:
      future_score = nscore
  return cell_score + future_score

def eval_cell(x, y, dist, map, world):
  star = get_my_vitality(world) / 150
  for ghost in get_ghosts(world):
    gx, gy = unpair(car(cdr(ghost)))
    if x == gx and y == gy:
      if dist < star:
        return 10000
      return -1000000
  cell = get_cell(x, y, map)
  if cell == 0:
    return -1000000
  if cell == 2:
    return 100
  if cell == 3:
    return 1000
  return 0

def get_cell(x, y, map):
  return get_2d_array(map, y, x)

@rank(2)
def adj_move(x, y, dir):
  if dir == 0:
    y -= 1
  elif dir == 1:
    x += 1
  elif dir == 2:
    y += 1
  elif dir == 3:
    x -= 1
  return x, y


###
### 2D-arrays
###

def convert_2d_array(map):
  h = len(map)
  w = len(map[0])
  array = make_array(h, make_array(w, 0))
  y = 0
  for row in map:
    x = 0
    for cell in row:
      array = set_2d_array(array, y, x, cell)
      x += 1
    y += 1
  return array

@nolocals
def set_2d_array(array, i, j, value):
  return set_array(array, i, set_array(get_array(array, i), j, value))

@nolocals
def get_2d_array(array, i, j):
  return get_array(get_array(array, i), j)


###
### world accessors
###

@rank(2)
def get_my_pos(world):
  return unpair(car(cdr(car(cdr(world)))))

def get_my_dir(world):
  return car(cdr(cdr(car(cdr(world)))))

def get_my_vitality(world):
  return car(car(cdr(world)))

def get_ghosts(world):
  return car(cdr(cdr(world)))

def get_map(world):
  return car(world)


###
### immutable arrays
###

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

@asm
@rank(1)
def get_array(array, index):
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
