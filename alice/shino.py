#!/usr/bin/python

"""Shino - GCC Linker."""

import collections
import os
import re
import sys

STUB_HEADER = """
LDC 0
LDF main
AP 1
RTN
main:
"""

STUB_ENTRY = """
try_%:
LD 1 0
LD 1 1
LDF %
AP 2
ST 0 0
LD 0 0
ATOM
TSEL try_% go_%
go_%:
LDC #
DBUG
LDC 283
TSEL start start
"""

STUB_FOOTER = """
start:
LDC 55555555
DBUG
LD 0 0
RTN
"""

def resolve_labels(asm):
  lines = asm.splitlines()
  pc = 0
  label_map = collections.OrderedDict()
  ops = []
  for i, line in enumerate(lines):
    s = re.sub(r';.*', '', line).strip()
    if s.endswith(':'):
      label = s.strip(':')
      label_map[label] = str(pc)
    elif s:
      ops.append(s)
      pc += 1
  for i, op in enumerate(ops):
    ops[i] = ' '.join(label_map.get(s, s) for s in op.split())
  return '\n'.join(ops)


def modulize(asm, prefix):
  lines = asm.splitlines()
  pc = 0
  label_map = collections.OrderedDict()
  ops = []
  for i, line in enumerate(lines):
    s = re.sub(r';.*', '', line).strip()
    if s.endswith(':'):
      label = s.strip(':')
      newlabel = '%s__%s' % (prefix, label)
      label_map[label] = newlabel
      ops.append(newlabel + ':')
    elif s:
      ops.append(s)
      pc += 1
  for i, op in enumerate(ops):
    if not op.endswith(':'):
      ops[i] = ' '.join(label_map.get(s, s) for s in op.split())
  main_label = '%s_main' % prefix
  ops.insert(0, '%s:' % main_label)
  return '\n'.join(ops), main_label

def main():
  raw = False
  if len(sys.argv) >= 2 and sys.argv[1] == '-r':
    del sys.argv[1:2]
    raw = True
  main_labels = []
  all_codes = []
  for i in xrange(1, len(sys.argv)):
    path = sys.argv[i]
    id = i - 1
    with open(path) as f:
      code = f.read()
    name = os.path.splitext(os.path.basename(path))[0]
    code, main_label = modulize(code, 'module%d_%s' % (id, name))
    all_codes.append(code)
    main_labels.append(main_label)
  stub_code = STUB_HEADER
  for i, label in enumerate(main_labels):
    stub_code += STUB_ENTRY.replace('%', label).replace('#', str(i))
  stub_code += STUB_FOOTER
  linked_code = '\n'.join([stub_code] + all_codes)
  if not raw:
    linked_code = resolve_labels(linked_code)
  print linked_code


if __name__ == '__main__':
  main()
