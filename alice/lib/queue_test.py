#!/usr/bin/python

import define_alice_builtins
import unittest

import queue


def make_list(*args):
  xs = 0
  for x in reversed(args):
    xs = (x, xs)
  return xs


class QueueTestCase(unittest.TestCase):
  def test_make_queue(self):
    assert queue.make_queue() == (0, 0)

  def test_empty_queue(self):
    assert queue.empty_queue((make_list(), make_list()))
    assert not queue.empty_queue((make_list(28), make_list()))
    assert not queue.empty_queue((make_list(), make_list(28)))
    assert not queue.empty_queue((make_list(28), make_list(28)))

  def test_push_queue(self):
    assert queue.push_queue((make_list(10), make_list(20)), 28) == (make_list(28, 10), make_list(20))

  def test_pop_queue(self):
    assert (queue.pop_queue((make_list(6, 5, 4), make_list(1, 2, 3))) ==
            ((make_list(6, 5, 4), make_list(2, 3)), 1))
    assert (queue.pop_queue((make_list(6, 5, 4), make_list(3))) ==
            ((make_list(6, 5, 4), make_list()), 3))
    assert (queue.pop_queue((make_list(6, 5, 4), make_list())) ==
            ((make_list(), make_list(5, 6)), 4))


if __name__ == '__main__':
  unittest.main()
