def main(world, _):
  return (0, step)

def step(_, world):
  return (0, decide(world))

def decide(world):
  cx, cy = get_pos(world)
  cdir = get_dir(world)
  best_dir = 0
  best_score = -1000000
  for dir in xrange(4):
    score = eval_move(adj_move(cx, cy, dir), dir, 0, 1, world)
    if dir == (cdir + 2) % 4:
      score -= 1
    print dir, score
    if score > best_score:
      best_dir = dir
      best_score = score
  return best_dir

def eval_move(cx, cy, cdir, branches, dist, world):
  cell_score = eval_cell(cx, cy, dist, world)
  if cell_score < 0:
    return cell_score
  future_score = -1000000
  choices = []
  for ndir in range(4):
    if ndir == (cdir + 2) % 4:
      continue
    if get_cell(adj_move(cx, cy, ndir), world) > 0:
      choices = (ndir, choices)  # prepend
  if len(choices) >= 2:
    branches += 1
    if branches >= 3:
      return cell_score
  for ndir in choices:
    nscore = eval_move(adj_move(cx, cy, ndir), ndir, branches, dist + 1, world)
    if nscore > future_score:
      future_score = nscore
  return cell_score + future_score

def eval_cell(x, y, dist, world):
  star = get_vitality(world) / 150
  for ghost in get_ghosts(world):
    gx, gy = unpair(car(cdr(ghost)))
    if x == gx and y == gy:
      if dist < star:
        return 10000
      return -1000000
  cell = get_cell(x, y, world)
  if cell == 0:
    return -1000000
  if cell == 2:
    return 100
  if cell == 3:
    return 1000
  return 0

def get_ghosts(world):
  return car(cdr(cdr(world)))

def get_cell(x, y, world):
  return car(world)[y][x]

def get_vitality(world):
  return car(car(cdr(world)))

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

def get_dir(world):
  return car(cdr(cdr(car(cdr(world)))))

@rank(2)
def get_pos(world):
  return unpair(car(cdr(car(cdr(world)))))
