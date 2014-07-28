#!/usr/bin/python

import json
import glob
import os
import re
import subprocess
import sys
import threading
import time

import gflags

FLAGS = gflags.FLAGS

gflags.DEFINE_string('data_dir', None, '')
gflags.DEFINE_string('ghost_data_dir', None, '')
gflags.DEFINE_string('remote_host', None, '')
gflags.DEFINE_string('remote_root', None, '')
gflags.DEFINE_integer('time_limit', 180, '')
gflags.MarkFlagAsRequired('data_dir')
gflags.MarkFlagAsRequired('ghost_data_dir')
gflags.MarkFlagAsRequired('remote_host')
gflags.MarkFlagAsRequired('remote_root')
gflags.MarkFlagAsRequired('time_limit')


g_lock = threading.Lock()


def rate_limit():
  with g_lock:
    time.sleep(0.25)


def list_evalsets(evalset_kind):
  return [os.path.splitext(shname)[0]
          for shname
          in os.listdir(os.path.join(os.path.dirname(__file__), evalset_kind))
          if shname.endswith('.sh')]


class Job(object):
  def __init__(self, data_dir, evalset_kind, key, evalset):
    self.data_dir = data_dir
    self.evalset_kind = evalset_kind
    self.key = key
    self.evalset = evalset
    self.proc = None

  def start(self):
    assert not self.proc
    rate_limit()
    print 'starting', self.key, self.evalset
    script = os.path.join(
        FLAGS.remote_root,
        'eval_server',
        self.evalset_kind,
        '%s.sh' % self.evalset)
    jsonpath = os.path.join(
        self.data_dir, '%s.response.%s.json' % (self.key, self.evalset))
    codepath = os.path.join(
        self.data_dir,  '%s.request.code' % self.key)
    stdoutpath = os.path.join(
        self.data_dir, '%s.response.%s.stdout.gz' % (self.key, self.evalset))
    stderrpath = os.path.join(
        self.data_dir, '%s.response.%s.stderr' % (self.key, self.evalset))
    self.json_out = open(jsonpath, 'w')
    with open(codepath) as f:
      code = f.read()
    with open(stdoutpath, 'w') as stdout:
      with open(stderrpath, 'w') as stderr:
        self.proc = subprocess.Popen(
            'ssh %s "(ulimit -t %d; time %s) | tail -n 100000 | gzip"' % (FLAGS.remote_host, FLAGS.time_limit, script),
            shell=True,
            stdin=subprocess.PIPE, stdout=stdout, stderr=stderr)
    self.proc.stdin.write(code)
    self.proc.stdin.close()

  def poll(self):
    assert self.proc
    returncode = self.proc.poll()
    if returncode is None:
      return False
    print 'finished', self.key, self.evalset
    stdoutpath = os.path.join(
        self.data_dir, '%s.response.%s.stdout.gz' % (self.key, self.evalset))
    line = subprocess.check_output('zcat "%s" | grep "========" | tail -n 1' % stdoutpath, shell=True).strip()
    try:
      time = int(re.search(r'\[(\d+)\]', line).group(1))
      score = int(re.search(r'Score=(\d+)', line).group(1))
    except Exception:
      time, score = -1, -1
    data = {
        'key': self.key,
        'evalset': self.evalset,
        'time': time,
        'score': score,
        }
    json.dump(data, self.json_out, indent=2, sort_keys=True)
    self.json_out.close()
    self.json_out = None
    self.proc = None
    return True


class State(object):
  def __init__(self, key):
    self.key = key
    self.date = 'N/A'
    self.queue = []
    self.current_job = None


def scan_requests(data_dir, evalset_kind, pool):
  for jsonpath in glob.glob(os.path.join(data_dir, '*.request.json')):
    with open(jsonpath) as f:
      entry = json.load(f)
    key = entry['key']
    date = entry['date']
    if key not in pool:
      pool[key] = State(key)
    state = pool[key]
    if state.date != date:
      print 'accepted', key
      state.queue = []
      state.date = date
      for evalset in list_evalsets(evalset_kind):
        jsonpath = os.path.join(data_dir, '%s.response.%s.json' % (key, evalset))
        if not os.path.exists(jsonpath):
          state.queue.append(Job(data_dir, evalset_kind, key, evalset))
      state.queue.sort(key=lambda job: job.evalset)


def poll_jobs(pool):
  for state in pool.itervalues():
    if state.current_job:
      if state.current_job.poll():
        state.current_job = None
    if not state.current_job and state.queue:
      state.current_job = state.queue.pop(0)
      state.current_job.start()


def main(argv):
  pool = {}
  ghost_pool = {}
  while True:
    scan_requests(FLAGS.data_dir, 'evalsets', pool)
    scan_requests(FLAGS.ghost_data_dir, 'ghost_evalsets', ghost_pool)
    poll_jobs(pool)
    poll_jobs(ghost_pool)
    time.sleep(3)


if __name__ == '__main__':
  sys.argv = FLAGS(sys.argv)
  main(sys.argv)
