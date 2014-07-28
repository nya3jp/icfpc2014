def main():
  print fact(10)

@asm
@rank(1)
def fact(n):
  """
  LD 0 0
  TSEL %nonzero %zero
%nonzero:
  LD 0 0
  LD 0 0
  LDC 1
  SUB
  LDF fact
  AP 1
  MUL
  RTN
%zero:
  LDC 1
  RTN
  """
