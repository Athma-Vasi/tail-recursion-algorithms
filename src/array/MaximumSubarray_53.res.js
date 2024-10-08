// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Int32 from "rescript/lib/es6/int32.js";

function maximumSubarray(nums) {
  var _max = Int32.min_int;
  var _sum = 0;
  var _index = 0;
  while(true) {
    var index = _index;
    var sum = _sum;
    var max = _max;
    if (index === nums.length) {
      return max;
    }
    var n = nums.at(index);
    var num = n !== undefined ? n : 0;
    var newSum = sum + num | 0;
    _index = index + 1 | 0;
    _sum = newSum > 0 ? newSum : 0;
    _max = max > newSum ? max : newSum;
    continue ;
  };
}

var n1 = [
  -2,
  1,
  -3,
  4,
  -1,
  2,
  1,
  -5,
  4
];

var r1 = maximumSubarray(n1);

console.log("r1: ", r1);

var n2 = [1];

var r2 = maximumSubarray(n2);

console.log("r2: ", r2);

export {
  maximumSubarray ,
  n1 ,
  r1 ,
  n2 ,
  r2 ,
}
/* r1 Not a pure module */
