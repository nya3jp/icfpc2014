#!/usr/bin/python

import collections
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


def process_results(entries):
  evalset_score_max = collections.defaultdict(lambda: 10)
  for entry in entries:
    for result in entry['results']:
      evalset = result['evalset']
      evalset_score_max[evalset] = max(evalset_score_max[evalset], result['score'])
  evalsets = sorted(evalset_score_max)
  for entry in entries:
    results_map = {}
    for result in entry['results']:
      results_map[result['evalset']] = result
    entry['results'] = [results_map.get(evalset) for evalset in evalsets]
    for result in entry['results']:
      if result:
        result['score_ratio'] = 1.0 * result['score'] / evalset_score_max[result['evalset']]
        result['score_percent'] = int(result['score_ratio'] * 100)
        result['winner'] = (result['score'] == evalset_score_max[result['evalset']])
    scores = [result['score_ratio'] for result in entry['results'] if result and result['score'] >= 0]
    entry['score_ratio'] = sum(scores) / len(scores)
    entry['score_percent'] = int(100 * entry['score_ratio'])
  entries.sort(key=lambda entry: entry['score_ratio'], reverse=True)
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
  evalsets = process_results(entries)
  return bottle.template(template_name, entries=entries, evalsets=evalsets)


def handle_submit(data_dir):
  key = bottle.request.forms['key']
  code = bottle.request.files['code'].file.read()
  assert re.search(r'^[a-zA-z0-9_,-]+$', key)
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
