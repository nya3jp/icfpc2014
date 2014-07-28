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
