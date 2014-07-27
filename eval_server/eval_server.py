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
gflags.DEFINE_string('ghost_data_dir', None, '')
gflags.MarkFlagAsRequired('port')
gflags.MarkFlagAsRequired('data_dir')
gflags.MarkFlagAsRequired('ghost_data_dir')


def load_entry(name):
  with open(os.path.join(FLAGS.data_dir, '%s.request.json' % name)) as f:
    entry = json.load(f)
  entry['results'] = []
  for jsonname in os.listdir(FLAGS.data_dir):
    if jsonname.startswith(name + '.response.') and jsonname.endswith('.json'):
      jsonpath = os.path.join(FLAGS.data_dir, jsonname)
      with open(jsonpath) as f:
        try:
          entry['results'].append(json.load(f))
        except ValueError:
          continue
  return entry


def load_ghost_entry(name):
  with open(os.path.join(FLAGS.ghost_data_dir, '%s.request.json' % name)) as f:
    entry = json.load(f)
  entry['results'] = []
  for jsonname in os.listdir(FLAGS.ghost_data_dir):
    if jsonname.startswith(name + '.response.') and jsonname.endswith('.json'):
      jsonpath = os.path.join(FLAGS.ghost_data_dir, jsonname)
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


@bottle.get('/')
def index_handler():
  entries = []
  for jsonname in sorted(os.listdir(FLAGS.data_dir), reverse=True):
    if jsonname.endswith('.request.json'):
      jsonpath = os.path.join(FLAGS.data_dir, jsonname)
      with open(jsonpath) as f:
        try:
          entry = load_entry(json.load(f)['name'])
        except ValueError:
          continue
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
  name = '%s-%s' % (now.strftime('%Y%m%d-%H%M%S-%f'), user)
  data = {
      'name': name,
      'title': now.strftime('%Y-%m-%d %H:%M:%S') + ' by ' + user,
      'user': user,
      'url': url,
      'comment': comment,
      }
  with open(os.path.join(FLAGS.data_dir, '%s.request.json' % name), 'w') as f:
    json.dump(data, f, indent=2, sort_keys=True)
  with open(os.path.join(FLAGS.data_dir, '%s.request.code' % name), 'w') as f:
    f.write(code)
  return bottle.redirect('./')


@bottle.get('/ghost/')
def ghost_index_handler():
  entries = []
  for jsonname in sorted(os.listdir(FLAGS.ghost_data_dir), reverse=True):
    if jsonname.endswith('.request.json'):
      jsonpath = os.path.join(FLAGS.ghost_data_dir, jsonname)
      with open(jsonpath) as f:
        try:
          entry = load_ghost_entry(json.load(f)['name'])
        except ValueError:
          continue
      entries.append(entry)
  evalsets = flatten_results(entries)
  return bottle.template('ghost.html', entries=entries, evalsets=evalsets)


@bottle.post('/ghost/submit')
def submit_handler():
  user = bottle.request.forms['user']
  url = bottle.request.forms['url']
  comment = bottle.request.forms['comment']
  code = bottle.request.files['code'].file.read()
  assert re.search(r'^[a-zA-z0-9_-]+$', user)
  now = datetime.datetime.now()
  name = '%s-%s' % (now.strftime('%Y%m%d-%H%M%S-%f'), user)
  data = {
      'name': name,
      'title': now.strftime('%Y-%m-%d %H:%M:%S') + ' by ' + user,
      'user': user,
      'url': url,
      'comment': comment,
      }
  with open(os.path.join(FLAGS.ghost_data_dir, '%s.request.json' % name), 'w') as f:
    json.dump(data, f, indent=2, sort_keys=True)
  with open(os.path.join(FLAGS.ghost_data_dir, '%s.request.code' % name), 'w') as f:
    f.write(code)
  return bottle.redirect('./')


if __name__ == '__main__':
  sys.argv = FLAGS(sys.argv)
  bottle.debug()
  bottle.run(host='0.0.0.0', port=FLAGS.port)
