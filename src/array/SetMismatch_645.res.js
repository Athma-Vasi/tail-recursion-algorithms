// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Int32 from "rescript/lib/es6/int32.js";

function setMismatch(nums) {
  var n = nums.at(0);
  var first = n !== undefined ? n : 0;
  var _result = [];
  var _index = 1;
  var _stack = [first];
  while(true) {
    var stack = _stack;
    var index = _index;
    var result = _result;
    if (index === nums.length) {
      return result;
    }
    var n$1 = nums.at(index);
    var curr = n$1 !== undefined ? n$1 : 0;
    var n$2 = stack.at(-1);
    var prev = n$2 !== undefined ? n$2 : Int32.min_int;
    if (curr === (prev + 1 | 0)) {
      _stack = stack.concat([curr]);
      _index = index + 1 | 0;
      continue ;
    }
    _stack = stack.concat([curr + 1 | 0]);
    _index = index + 1 | 0;
    _result = result.concat([
          prev,
          curr + 1 | 0
        ]);
    continue ;
  };
}

var n1 = [
  1,
  2,
  2,
  4
];

var r1 = setMismatch(n1);

console.log("r1: ", r1);

var n2 = [
  1,
  1
];

var r2 = setMismatch(n2);

console.log("r2: ", r2);

export {
  setMismatch ,
  n1 ,
  r1 ,
  n2 ,
  r2 ,
}
/* r1 Not a pure module */