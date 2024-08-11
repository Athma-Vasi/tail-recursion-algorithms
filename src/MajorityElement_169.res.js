// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Int32 from "rescript/lib/es6/int32.js";

function majorityElement(nums) {
  var n = nums.at(0);
  var candidate = n !== undefined ? n : Int32.min_int;
  var _count = 1;
  var _index = 1;
  var _candidate = candidate;
  while(true) {
    var candidate$1 = _candidate;
    var index = _index;
    var count = _count;
    if (index === nums.length) {
      return candidate$1;
    }
    var n$1 = nums.at(index);
    var num = n$1 !== undefined ? n$1 : Int32.min_int;
    if (num === candidate$1) {
      _index = index + 1 | 0;
      _count = count + 1 | 0;
      continue ;
    }
    if (count === 0) {
      _candidate = num;
      _index = index + 1 | 0;
      _count = 1;
      continue ;
    }
    _index = index + 1 | 0;
    _count = count - 1 | 0;
    continue ;
  };
}

var n1 = [
  3,
  2,
  3
];

var r1 = majorityElement(n1);

console.log("r1: ", r1);

var n2 = [
  2,
  2,
  1,
  1,
  1,
  2,
  2
];

var r2 = majorityElement(n2);

console.log("r2: ", r2);

export {
  majorityElement ,
  n1 ,
  r1 ,
  n2 ,
  r2 ,
}
/* r1 Not a pure module */
