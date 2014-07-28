#!/usr/bin/python

import datetime
import json
import glob
import os
import re
import subprocess
import sys

import bottle
import gflags

FLAGS = gflags.FLAGS

gflags.DEFINE_integer('port', None, '')
gflags.DEFINE_string('data_dir', None, '')
gflags.DEFINE_string('ghost_data_dir', None, '')
gflags.MarkFlagAsRequired('port')
gflags.MarkFlagAsRequired('data_dir')
gflags.MarkFlagAsRequired('ghost_data_dir')


def load_entry(key, data_dir):
  with open(os.path.join(data_dir, '%s.request.json' % key)) as f:
    entry = json.load(f)
  entry['results'] = []
  for jsonpath in glob.glob(os.path.join(data_dir, '%s.response.*.json' % key)):
    with open(jsonpath) as f:
      try:
        entry['results'].append(json.load(f))
      except ValueError:
        continue
  return entry


def flatten_results(entries):
  evalsets = set()
  for entry in entries:
    for result in entry['results']:
      evalsets.add(result['evalset'])
  evalsets = sorted(evalsets)
  for entry in entries:
    results_map = {}
    for result in entry['results']:
      results_map[result['evalset']] = result
    entry['results'] = [results_map.get(evalset) for evalset in evalsets]
  return evalsets


def handle_index(data_dir, template_name):
  entries = []
  for jsonpath in glob.glob(os.path.join(data_dir, '*.request.json')):
    with open(jsonpath) as f:
      try:
        entry = load_entry(json.load(f)['key'], data_dir)
      except ValueError:
        continue
      entries.append(entry)
  evalsets = flatten_results(entries)
  return bottle.template(template_name, entries=entries, evalsets=evalsets)


def handle_submit(data_dir):
  key = bottle.request.forms['key']
  code = bottle.request.files['code'].file.read()
  assert re.search(r'^[a-zA-z0-9_-]+$', key)
  now = datetime.datetime.now()
  data = {
      'key': key,
      'date': now.strftime('%Y-%m-%d %H:%M:%S'),
      }
  prefix = os.path.join(data_dir, key)
  subprocess.check_call('rm -f "%s".*' % prefix, shell=True)
  with open(prefix + '.request.code', 'w') as f:
    f.write(code)
  with open(prefix + '.request.json', 'w') as f:
    json.dump(data, f, indent=2, sort_keys=True)
  return bottle.redirect('./')


@bottle.get('/')
def index_handler():
  return handle_index(FLAGS.data_dir, 'index.html')


@bottle.post('/submit')
def submit_handler():
  handle_submit(FLAGS.data_dir)


@bottle.get('/ghost/')
def ghost_index_handler():
  return handle_index(FLAGS.ghost_data_dir, 'index.html')


@bottle.post('/ghost/submit')
def submit_handler():
  return handle_submit(FLAGS.ghost_data_dir)


if __name__ == '__main__':
  sys.argv = FLAGS(sys.argv)
  bottle.debug()
  bottle.run(host='0.0.0.0', port=FLAGS.port)
