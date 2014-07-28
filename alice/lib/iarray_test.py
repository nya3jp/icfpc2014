#!/usr/bin/python

import define_alice_builtins
import unittest

import iarray


def make_list(*args):
  xs = 0
  for x in reversed(args):
    xs = (x, xs)
  return xs


class ArrayTestCase(unittest.TestCase):
  def test_make_array(self):
    assert iarray.make_array(1, 28) == (1, 28)
    assert iarray.make_array(2, 28) == (2, (28, 28))
    assert iarray.make_array(3, 28) == (4, ((28, 28), (28, 28)))

  def test_get_array(self):
    ar = (4, ((10, 11), (12, 13)))
    assert iarray.get_array(ar, 0) == 10
    assert iarray.get_array(ar, 1) == 11
    assert iarray.get_array(ar, 2) == 12
    assert iarray.get_array(ar, 3) == 13
    ar = (1, 10)
    assert iarray.get_array(ar, 0) == 10

  def test_set_array(self):
    ar = (4, ((10, 11), (12, 13)))
    assert iarray.set_array(ar, 0, 28) == (4, ((28, 11), (12, 13)))
    assert iarray.set_array(ar, 1, 28) == (4, ((10, 28), (12, 13)))
    assert iarray.set_array(ar, 2, 28) == (4, ((10, 11), (28, 13)))
    assert iarray.set_array(ar, 3, 28) == (4, ((10, 11), (12, 28)))
    ar = (1, 10)
    assert iarray.set_array(ar, 0, 28) == (1, 28)


if __name__ == '__main__':
  unittest.main()
