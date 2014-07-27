#!/usr/bin/python

import json
import os
import subprocess
import sys
import threading
import time

import gflags
import workerpool

FLAGS = gflags.FLAGS

gflags.DEFINE_string('data_dir', None, '')
gflags.DEFINE_string('evalsets', None, '')
gflags.DEFINE_string('remote_host', None, '')
gflags.DEFINE_string('remote_root', None, '')
gflags.DEFINE_integer('pool_size', None, '')
gflags.MarkFlagAsRequired('data_dir')
gflags.MarkFlagAsRequired('evalsets')
gflags.MarkFlagAsRequired('pool_size')
gflags.MarkFlagAsRequired('remote_host')
gflags.MarkFlagAsRequired('remote_root')


g_lock = threading.Lock()


class EvalJob(object):
  def __init__(self, name, evalset):
    self.name = name
    self.evalset = evalset

  def run(self):
    with g_lock:
      time.sleep(1)
    print 'started', self.name, self.evalset
    script = os.path.join(FLAGS.remote_root, 'eval_server', 'evalsets', '%s.sh' % self.evalset)
    codepath = os.path.join(
        FLAGS.data_dir, '%s.request.code' % self.name)
    jsonpath = os.path.join(
        FLAGS.data_dir, '%s.response.%s.json' % (self.name, self.evalset))
    stdoutpath = os.path.join(
        FLAGS.data_dir, '%s.response.%s.stdout' % (self.name, self.evalset))
    stderrpath = os.path.join(
        FLAGS.data_dir, '%s.response.%s.stderr' % (self.name, self.evalset))
    with open(codepath) as f:
      code = f.read()
    with open(stdoutpath, 'w') as stdout:
      with open(stderrpath, 'w') as stderr:
        p = subprocess.Popen(
            'ssh %s "%s 2>&1 | gzip" | gzip -d' % (FLAGS.remote_host, script),
            shell=True,
            stdin=subprocess.PIPE, stdout=stdout, stderr=stderr)
    p.stdin.write(code)
    p.stdin.close()
    p.wait()
    score = subprocess.check_output('cat "%s" | tail -n 1' % stdoutpath, shell=True).strip()
    try:
      score = int(score)
    except ValueError:
      score = -1
    res = {
        'name': self.name,
        'evalset': self.evalset,
        'score': score,
        }
    with open(jsonpath, 'w') as f:
      json.dump(res, f, indent=2, sort_keys=True)
    print 'finished', self.name, self.evalset


def main(argv):
  pool = workerpool.WorkerPool(size=FLAGS.pool_size)
  while True:
    time.sleep(3)
    for jsonname in os.listdir(FLAGS.data_dir):
      if jsonname.endswith('.request.json'):
        jsonpath = os.path.join(FLAGS.data_dir, jsonname)
        with open(jsonpath) as f:
          entry = json.load(f)
        name = entry['name']
        for evalset in FLAGS.evalsets.split(','):
          jsonpath = os.path.join(
              FLAGS.data_dir, '%s.response.%s.json' % (name, evalset))
          if not os.path.exists(jsonpath):
            with open(jsonpath, 'w'):
              pass
            job = EvalJob(name, evalset)
            pool.put(job)
            print 'submitted', name, evalset


if __name__ == '__main__':
  sys.argv = FLAGS(sys.argv)
  main(sys.argv)
