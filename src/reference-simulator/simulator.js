// Reference Simulator CUI.
//
// Usage:
// npm install minimist
// node simulator.js --map=map.txt --lambda=lambda.txt --ghost=ghost.txt

var fs = require('fs');
var flags = require('minimist')(process.argv.slice(2));
var game = require('./game.js');

var TILES = '# .o%@\\ABCDEFG';

var checkFlagsOrDie = function() {
  var abort = false;
  if (!flags.map) {
    console.log('FATAL: --map is required');
    abort = true;
  }
  if (!flags.lambda) {
    console.log('FATAL: --lambda is required');
    abort = true;
  }
  if (!flags.ghost) {
    console.log('FATAL: --ghost is required');
    abort = true;
  }
  if (abort) {
    console.log('Usage: node simulator.js --map=map.txt --lambda=lambda.txt --ghost=ghost.txt');
    process.exit(1);
  }
};

var initializeOrDie = function() {
  checkFlagsOrDie();

  var map = fs.readFileSync(flags.map, {encoding: 'utf-8'});
  var lambda = fs.readFileSync(flags.lambda, {encoding: 'utf-8'});
  var ghosts = [];
  flags.ghost.split(',').forEach(function(filename) {
    ghosts.push(fs.readFileSync(filename, {encoding: 'utf-8'}));
  });

  return game.init(map, lambda, ghosts);
};

var printBoard = function(state) {
  for (var y = 0; y < state.board.length; ++y) {
    var row = state.board[y];
    var line = '';
    for (var x = 0; x < row.length; ++x) {
      line += TILES.charAt(row[x]);
    }
    console.log(line);
  }
};

var printState = function(state) {
  if (state.traceval) {
    for (var i = 0; i < state.traceval.length; ++i) {
      console.log('; %s', state.traceval[i]);
    }
  }
  console.log('====================================== [%s] Lives=%s Score=%s',
              state.ticks, state.lives, state.score);
  printBoard(state);
  console.log();
};

var main = function() {
  var state = initializeOrDie();

  while (true) {
    if (state.error) {
      console.log(state.error);
      break;
    }
    printState(state);
    if (state.gameOver) {
      var victor = state.gameWin ? 'You won' : 'You lost';
      console.log('＿人人人人人＿\n＞ %s ＜\n￣Y^Y^Y^Y￣', victor);
      break;
    }
    game.step(state);
  }

  console.log(state.score);
};

main();
