def main():
  for i in xrange(1, 10, 2):
    if i == 5:
      continue
    print i
  j = 1
  while j < 10:
    print j
    if j == 5:
      break
    j += 2
  for k in [2, 8, 3]:
    print k
