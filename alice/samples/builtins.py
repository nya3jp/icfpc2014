def main():
  # indexing
  v = [1, 2, 3, 4, 5]
  print v[2]

  # unpair
  p = (28, 3)
  a, b = unpair(p)
  print a
  print b

  # car/cdr
  a = car(p)
  b = cdr(p)
  print a
  print b
