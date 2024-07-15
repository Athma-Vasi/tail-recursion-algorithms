// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Int32 from "rescript/lib/es6/int32.js";

function maxConsecutiveOnesII(binaryArray) {
  var length = binaryArray.length;
  var _maxLength = Int32.min_int;
  var _counter = 0;
  var _index = 0;
  while(true) {
    var index = _index;
    var counter = _counter;
    var maxLength = _maxLength;
    var b = binaryArray[index];
    var binary = b !== undefined ? b : 0;
    var newMaxLength = maxLength > counter ? maxLength : counter;
    if (index === length) {
      return newMaxLength;
    }
    if (binary === 0) {
      _index = index + 1 | 0;
      _counter = 0;
      _maxLength = newMaxLength;
      continue ;
    }
    _index = index + 1 | 0;
    _counter = counter + 1 | 0;
    continue ;
  };
}

var b1 = [
  1,
  1,
  0,
  1,
  1,
  1
];

var r1 = maxConsecutiveOnesII(b1);

console.log("[1,1,0,1,1,1]", r1);

var b2 = [
  1,
  0,
  1,
  1,
  0
];

var r2 = maxConsecutiveOnesII(b2);

console.log("[1,0,1,1,0]", r2);

export {
  maxConsecutiveOnesII ,
  b1 ,
  r1 ,
  b2 ,
  r2 ,
}
/* r1 Not a pure module */
