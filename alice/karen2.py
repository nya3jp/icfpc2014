###
### How is the progress?
###

def main(world, ghosts):
  cycle = 0
  tactics = 0
  map = convert_2d_array(car(world))
  white = make_white_map(map)
  state = (cycle, tactics, map, white)
  return (state, step)

def step(state, world):
  cycle = car(state)
  tactics = car(cdr(state))
  map = car(cdr(cdr(state)))
  white = cdr(cdr(cdr(state)))
  self = cdr(car(cdr(world)))
  cx = car(car(self))
  cy = cdr(car(self))
  map = set_map_cell(map, cx, cy, 1)  # update map
  globals = make_globals(world, map, white, cycle)
  tactics, dir = decide(tactics, globals)
  print dir
  state = (cycle + 1, tactics, map, white)
  return (state, dir)

@rank(2)
def decide(tactics, globals):
  print tactics
  tactics = maybe_change_tactics(tactics, globals)
  print tactics
  if tactics == 0:
    dir = classic_decide(globals)
  if tactics == 1:
    dir = evader_decide(globals)
  return tactics, dir

def maybe_change_tactics(tactics, globals):
  safety = compute_safety(globals)
  if tactics == 0:
    if safety <= 2:
      return 1
  if tactics == 1:
    if safety >= 3:
      return 0
  return tactics

def compute_safety(globals):
  cx, cy, _ = get_self_once(globals)
  ghosts = make_normal_ghost_list(globals)
  for t in range(5):
    ghosts = simulate_player_move(ghosts, globals)
    for ghost in ghosts:
      gx = car(car(ghost))
      gy = cdr(car(ghost))
      if gx == cx and gy == cy:
        return t + 1
  return 5


###
### Tactics: halter
###

def halt_decide(globals):
  cx, cy, _ = get_self_once(globals)
  map = get_map(globals)
  for dir in range(4):
    if get_map_cell(map, adj(cx, cy, dir)) == 0:
      return dir
  print 999999  # BUG
  return 0


###
### Tactics: classic
###

def classic_decide(globals):
  cx, cy, cdir = get_self_once(globals)
  best_dir = 0
  best_score = -1000000
  for dir in xrange(4):
    score = classic_eval_move(adj(cx, cy, dir), dir, 0, 1, globals)
    if dir == (cdir + 2) % 4:
      score /= 2
    print dir, score
    if score > best_score:
      best_dir = dir
      best_score = score
  return best_dir

def classic_eval_move(cx, cy, cdir, branches, dist, globals):
  map = get_map(globals)
  cell_score = classic_eval_cell(cx, cy, dist, globals)
  if cell_score < 0:
    return cell_score
  choices = []
  for ndir in range(4):
    if ndir == (cdir + 2) % 4:
      continue
    if get_map_cell(map, adj(cx, cy, ndir)) > 0:
      choices = (ndir, choices)  # prepend
  nscore_sum = 0
  if atom(choices):
    choices = [(cdir + 2) % 4]  # deadend
    nscore_sum -= 4
  if len(choices) >= 2:
    branches += 1
    if branches >= 3:
      return cell_score
  #future_score = -1000000
  for ndir in choices:
    nscore = classic_eval_move(adj(cx, cy, ndir), ndir, branches, dist + 1, globals)
    #if nscore > future_score:
    #  future_score = nscore
    if nscore >= -10000:
      if nscore >= 0:
        nscore_sum += nscore
      nscore_sum -= 1
  nscore_sum /= len(choices)
  #return cell_score + future_score
  return cell_score + nscore_sum

def classic_eval_cell(x, y, dist, globals):
  star = get_vitality(globals) / 150
  for ghost in get_ghosts(globals):
    gx, gy = unpair(car(cdr(ghost)))
    if x == gx and y == gy:
      if dist < star:
        return 10000
      return -10000
  cell = get_map_cell(get_map(globals), x, y)
  if cell == 0:
    return -1000000
  if cell == 2:
    return 100
  if cell == 3:
    return 1000
  return 10


###
### Tactics: evader
###

def evader_decide(globals):
  print 119119119
  best_dir = 0
  best_score = -1
  for dir in range(4):
    score = evader_try(dir, globals)
    print dir, score
    if score > best_score:
      best_dir = dir
      best_score = score
  return best_dir

def evader_try(idir, globals):
  cx, cy, _ = get_self_once(globals)
  ix, iy = adj(cx, cy, idir)
  if get_map_cell(get_map(globals), ix, iy) == 0:
    return -1
  lambdas = [(((ix, iy), idir), 0)]
  ghosts = make_normal_ghost_list(globals)
  ghost_map = update_player_map(get_white(globals), ghosts)
  lambdas = filter_invalid_players_with_score(lambdas, ghost_map)
  if atom(lambdas):
    return 0
  for t in range(5):
    ghosts = simulate_player_move(ghosts, globals)
    ghost_map = update_player_map(ghost_map, ghosts)
    lambdas = filter_invalid_players_with_score(lambdas, ghost_map)
    if atom(lambdas):
      return t + 1
    lambdas = simulate_player_with_score_move(lambdas, globals)
  score = 0
  for player in lambdas:
    score += cdr(player)
  return score

def simulate_player_move(current_players, globals):
  map = get_map(globals)
  next_players = []
  for player in current_players:
    for dir in range(4):
      if dir == (cdr(player) + 2) % 4:
        continue
      gx, gy = adj(car(car(player)), cdr(car(player)), dir)
      if get_map_cell(map, gx, gy):
        next_players = (((gx, gy), dir), next_players)
  return next_players

def simulate_player_with_score_move(current_players, globals):
  map = get_map(globals)
  next_players = []
  for player in current_players:
    for dir in range(4):
      if dir == (cdr(car(player)) + 2) % 4:
        continue
      gx, gy = adj(car(car(car(player))), cdr(car(car(player))), dir)
      cell = get_map_cell(map, gx, gy)
      if cell:
        score = cdr(player)
        if cell == 1:
          score += 10000
        else:
          score += 100
        next_players = ((((gx, gy), dir), score), next_players)
  return next_players

def update_player_map(player_map, players):
  for player in players:
    player_map = set_map_cell(player_map, car(car(player)), cdr(car(player)), 9)
  return player_map

def filter_invalid_players_with_score(players, map):
  filtered_players = []
  for player in players:
    x = car(car(car(player)))
    y = cdr(car(car(player)))
    if get_map_cell(map, x, y) == 0:
      filtered_players = (player, filtered_players)
  return filtered_players

def make_normal_ghost_list(globals):
  ghosts = []
  for gentry in get_ghosts(globals):
    if car(gentry) == 0:  # normal ghost
      ghosts = (cdr(gentry), ghosts)
  return ghosts


###
### Utilities
###

@nolocals
def get_map_cell(map, x, y):
  return get_array(get_array(map, y), x)

@nolocals
def set_map_cell(map, x, y, cell):
  return set_array(map, y, set_array(get_array(map, y), x, cell))

def make_white_map(map):
  return (car(map), _make_white_map_rec_y(car(map), cdr(map)))

@nolocals
def _make_white_map_rec_y(width, tree):
  if width == 1:
    return (car(tree), _make_white_map_rec_x(car(tree), cdr(tree)))
  return (_make_white_map_rec_y(width / 2, car(tree)),
          _make_white_map_rec_y(width / 2, cdr(tree)))

@nolocals
def _make_white_map_rec_x(width, tree):
  if width == 1:
    if tree == 0:
      return -1
    return 0
  return (_make_white_map_rec_x(width / 2, car(tree)),
          _make_white_map_rec_x(width / 2, cdr(tree)))

@rank(2)
@nolocals
def adj(x, y, dir):
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
### Queue
###

def make_queue():
  return (0, 0)

def empty_queue(q):
  return atom(car(q)) and atom(cdr(q))

def push_queue(q, x):
  return ((x, car(q)), cdr(q))

@rank(2)
def pop_queue(q):
  inbox = car(q)
  outbox = cdr(q)
  if atom(outbox):
    while not atom(inbox):
      x = car(inbox)
      inbox = cdr(inbox)
      outbox = (x, outbox)
  x = car(outbox)
  outbox = cdr(outbox)
  return (inbox, outbox), x


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
### global objects
###

def make_globals(world, map, white, cycle):
  map = convert_2d_array(car(world))
  pos = car(cdr(car(cdr(world))))
  dir = car(cdr(cdr(car(cdr(world)))))
  self = (pos, dir)
  vitality = car(car(cdr(world)))
  ghosts = car(cdr(cdr(world)))
  white = make_white_map(map)
  return (((map, self), (vitality, ghosts)), (white, cycle))

@nolocals
def get_map(globals):
  return car(car(car(globals)))

@nolocals
def get_self(globals):
  return cdr(car(car(globals)))

@rank(3)
def get_self_once(globals):
  self = get_self(globals)
  return car(car(self)), cdr(car(self)), cdr(self)

@nolocals
def get_vitality(globals):
  return car(cdr(car(globals)))

@nolocals
def get_ghosts(globals):
  return cdr(cdr(car(globals)))

@nolocals
def get_white(globals):
  return car(cdr(globals))

@nolocals
def get_cycle(globals):
  return cdr(cdr(globals))


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
