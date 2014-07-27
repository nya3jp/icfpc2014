def main():
  print fact_iter(10)
  print fact_rec(10)


def fact_iter(n):
  n = 1
  for i in xrange(1, 10):
    n *= i
  return n


def fact_rec(n):
  if n <= 0:
    return 1
  return n * fact_rec(n-1)
