#!/usr/bin/python

import datetime
import json
import os
import re
import sys

import bottle
import gflags

FLAGS = gflags.FLAGS

gflags.DEFINE_integer('port', None, '')
gflags.DEFINE_string('data_dir', None, '')
gflags.MarkFlagAsRequired('port')
gflags.MarkFlagAsRequired('data_dir')


def load_entry(name):
  with open(os.path.join(FLAGS.data_dir, '%s.request.json' % name)) as f:
    entry = json.load(f)
  entry['results'] = []
  for jsonname in os.listdir(FLAGS.data_dir):
    if jsonname.startswith(name + '.response.') and jsonname.endswith('.json'):
      jsonpath = os.path.join(FLAGS.data_dir, jsonname)
      with open(jsonpath) as f:
        entry['results'].append(json.load(f))
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
    entry['results'] = [results_map.get(result['evalset']) for result in entry['results']]
  return evalsets


@bottle.get('/')
def index_handler():
  entries = []
  for jsonname in sorted(os.listdir(FLAGS.data_dir), reverse=True):
    if jsonname.endswith('.request.json'):
      jsonpath = os.path.join(FLAGS.data_dir, jsonname)
      with open(jsonpath) as f:
        entry = load_entry(json.load(f)['name'])
      entries.append(entry)
  evalsets = flatten_results(entries)
  return bottle.template('index.html', entries=entries, evalsets=evalsets)


@bottle.post('/submit')
def submit_handler():
  user = bottle.request.forms['user']
  url = bottle.request.forms['url']
  comment = bottle.request.forms['comment']
  code = bottle.request.files['code'].file.read()
  assert re.search(r'^[a-zA-z0-9_-]+$', user)
  now = datetime.datetime.now()
  name = '%s-%s' % (now.strftime('%Y%m%d%H%M%S'), user)
  data = {
      'name': name,
      'user': user,
      'url': url,
      'comment': comment,
      }
  with open(os.path.join(FLAGS.data_dir, '%s.request.json' % name), 'w') as f:
    json.dump(data, f, indent=2, sort_keys=True)
  with open(os.path.join(FLAGS.data_dir, '%s.request.code' % name), 'w') as f:
    f.write(code)
  return bottle.redirect('./')


if __name__ == '__main__':
  sys.argv = FLAGS(sys.argv)
  bottle.debug()
  bottle.run(host='0.0.0.0', port=FLAGS.port)
