// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Core__Int from "@rescript/core/src/Core__Int.res.js";
import * as Core__Array from "@rescript/core/src/Core__Array.res.js";

function baseballGame(ops) {
  var opsLength = ops.length;
  var _scores = [];
  var _index = 0;
  while(true) {
    var index = _index;
    var scores = _scores;
    var scoresLength = scores.length;
    if (index === opsLength) {
      return Core__Array.reduce(scores, 0, (function (acc, score) {
                    return acc + score | 0;
                  }));
    }
    var $$char = ops[index];
    if ($$char === undefined) {
      return -1;
    }
    if ($$char === "+") {
      var num = scores.at(-2);
      var leftScore = num !== undefined ? num : -1;
      var num$1 = scores.at(-1);
      var rightScore = num$1 !== undefined ? num$1 : -1;
      _index = index + 1 | 0;
      _scores = scores.concat([leftScore + rightScore | 0]);
      continue ;
    }
    if ($$char === "D") {
      var num$2 = scores.at(-1);
      var lastScore = num$2 !== undefined ? num$2 : -1;
      _index = index + 1 | 0;
      _scores = scores.concat([(lastScore << 1)]);
      continue ;
    }
    if ($$char === "C") {
      _index = index + 1 | 0;
      _scores = scores.slice(0, scoresLength - 1 | 0);
      continue ;
    }
    var $$int = Core__Int.fromString($$char, undefined);
    var intScore = $$int !== undefined ? $$int : -1;
    _index = index + 1 | 0;
    _scores = scores.concat([intScore]);
    continue ;
  };
}

var o1 = [
  "5",
  "2",
  "C",
  "D",
  "+"
];

var r1 = baseballGame(o1);

console.log("r1: ", r1);

var o2 = [
  "5",
  "-2",
  "4",
  "C",
  "D",
  "9",
  "+",
  "+"
];

var r2 = baseballGame(o2);

console.log("r2: ", r2);

var o3 = ["1"];

var r3 = baseballGame(o3);

console.log("r3: ", r3);

export {
  baseballGame ,
  o1 ,
  r1 ,
  o2 ,
  r2 ,
  o3 ,
  r3 ,
}
/* r1 Not a pure module */
