// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Core__Array from "@rescript/core/src/Core__Array.res.js";

function relativeRanks(scores) {
  var updateMonoDecrStack = function (_monoDecrStack, tupleToPush, _tempStack) {
    while(true) {
      var tempStack = _tempStack;
      var monoDecrStack = _monoDecrStack;
      var stackLength = monoDecrStack.length;
      var idxOfNum = tupleToPush[1];
      var numToPush = tupleToPush[0];
      var t = monoDecrStack.at(-1);
      var match = t !== undefined ? t : [
          -1,
          -1
        ];
      if (numToPush < match[0]) {
        return [
                monoDecrStack.concat([[
                        numToPush,
                        idxOfNum
                      ]]),
                tempStack
              ];
      }
      _tempStack = tempStack.concat(monoDecrStack.slice(0, stackLength - 1 | 0));
      _monoDecrStack = monoDecrStack.slice(0, stackLength - 1 | 0).concat([[
              numToPush,
              idxOfNum
            ]]);
      continue ;
    };
  };
  var scoresLoop = function (_monoDecrStack, _scoreIndex, _tempStack) {
    while(true) {
      var tempStack = _tempStack;
      var scoreIndex = _scoreIndex;
      var monoDecrStack = _monoDecrStack;
      var s = scores.at(scoreIndex);
      var score = s !== undefined ? s : -1;
      var match = updateMonoDecrStack(monoDecrStack, [
            score,
            scoreIndex
          ], tempStack);
      if (scoreIndex === scores.length) {
        return [
                monoDecrStack,
                tempStack
              ];
      }
      _tempStack = match[1];
      _scoreIndex = scoreIndex + 1 | 0;
      _monoDecrStack = match[0];
      continue ;
    };
  };
  var match = scoresLoop([], 0, []);
  var flushTempLoop = function (_monoDecrStack, _index, _tempStack) {
    while(true) {
      var tempStack = _tempStack;
      var index = _index;
      var monoDecrStack = _monoDecrStack;
      var t = tempStack.at(index);
      var remainingTuple = t !== undefined ? t : [
          -1,
          -1
        ];
      if (tempStack.length === 0) {
        return monoDecrStack;
      }
      var match = updateMonoDecrStack(monoDecrStack, remainingTuple, []);
      _tempStack = match[1];
      _index = index === tempStack.length ? 0 : index + 1 | 0;
      _monoDecrStack = match[0];
      continue ;
    };
  };
  var sortedDescStack = flushTempLoop(match[0], 0, match[1]);
  var ranks = Core__Array.make(scores.length, "");
  var _stackIndex = 0;
  while(true) {
    var stackIndex = _stackIndex;
    var t = sortedDescStack.at(stackIndex);
    var match$1 = t !== undefined ? t : [
        -1,
        -1
      ];
    var scoreIndex = match$1[1];
    ranks[scoreIndex] = scoreIndex === 0 ? "Gold Medal" : (
        scoreIndex === 1 ? "Silver Medal" : (
            scoreIndex === 2 ? "Bronze Medal" : scoreIndex.toString()
          )
      );
    if (stackIndex === sortedDescStack.length) {
      return ranks;
    }
    _stackIndex = stackIndex + 1 | 0;
    continue ;
  };
}

var s1 = [
  5,
  4,
  3,
  2,
  1
];

var r1 = relativeRanks(s1);

console.log("r1: ", r1);

var s2 = [
  10,
  3,
  8,
  9,
  4
];

var r2 = relativeRanks(s2);

console.log("r2: ", r2);

var s3 = [
  1,
  2,
  3,
  4,
  5
];

var r3 = relativeRanks(s3);

console.log("r3: ", r3);

export {
  relativeRanks ,
  s1 ,
  r1 ,
  s2 ,
  r2 ,
  s3 ,
  r3 ,
}
/* r1 Not a pure module */
