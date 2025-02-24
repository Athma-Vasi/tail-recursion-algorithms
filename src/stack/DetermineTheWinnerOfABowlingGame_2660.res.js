// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Core__Option from "@rescript/core/src/Core__Option.res.js";

function calculateScore(player) {
  var _score = 0;
  var _stack = [];
  var _index = 0;
  while(true) {
    var index = _index;
    var stack = _stack;
    var score = _score;
    if (index === player.length) {
      return score;
    }
    var pinsHit = Core__Option.mapOr(player.at(index), -1, (function (p) {
            return p;
          }));
    var prevPinsHit = Core__Option.mapOr(stack.at(-1), -1, (function (p) {
            return p;
          }));
    var prevPrevPinsHit = Core__Option.mapOr(stack.at(-2), -1, (function (p) {
            return p;
          }));
    _index = index + 1 | 0;
    _stack = stack.concat([pinsHit]);
    _score = prevPinsHit === 10 || prevPrevPinsHit === 10 ? score + (pinsHit << 1) | 0 : score + pinsHit | 0;
    continue ;
  };
}

function determineTheWinnerOfABowlingGame(player1, player2) {
  var score1 = calculateScore(player1);
  var score2 = calculateScore(player2);
  if (score1 > score2) {
    return 1;
  } else if (score1 < score2) {
    return 2;
  } else {
    return 0;
  }
}

var p1 = [
  5,
  10,
  3,
  2
];

var p11 = [
  6,
  5,
  7,
  3
];

var r1 = determineTheWinnerOfABowlingGame(p1, p11);

console.log("r1: ", r1);

var p2 = [
  3,
  5,
  7,
  6
];

var p22 = [
  8,
  10,
  10,
  2
];

var r2 = determineTheWinnerOfABowlingGame(p2, p22);

console.log("r2: ", r2);

var p3 = [
  2,
  3
];

var p33 = [
  4,
  1
];

var r3 = determineTheWinnerOfABowlingGame(p3, p33);

console.log("r3: ", r3);

var p4 = [
  1,
  1,
  1,
  10,
  10,
  10,
  10
];

var p44 = [
  10,
  10,
  10,
  10,
  1,
  1,
  1
];

var r4 = determineTheWinnerOfABowlingGame(p4, p44);

console.log("r4: ", r4);

export {
  calculateScore ,
  determineTheWinnerOfABowlingGame ,
  p1 ,
  p11 ,
  r1 ,
  p2 ,
  p22 ,
  r2 ,
  p3 ,
  p33 ,
  r3 ,
  p4 ,
  p44 ,
  r4 ,
}
/* r1 Not a pure module */
