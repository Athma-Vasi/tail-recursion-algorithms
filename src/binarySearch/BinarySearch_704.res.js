// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Int32 from "rescript/lib/es6/int32.js";

function binarySearch(nums, target) {
  var length = nums.length;
  if (length === 0) {
    return -1;
  } else {
    var _leftIndex = 0;
    var _rightIndex = length - 1 | 0;
    while(true) {
      var rightIndex = _rightIndex;
      var leftIndex = _leftIndex;
      if (leftIndex > rightIndex) {
        return -1;
      }
      var middleIndex = leftIndex + ((rightIndex - leftIndex | 0) / 2 | 0) | 0;
      var num = nums.at(middleIndex);
      var middleNum = num !== undefined ? num : Int32.min_int;
      if (middleNum === Int32.min_int) {
        return -1;
      }
      if (middleNum === target) {
        return middleIndex;
      }
      if (middleNum > target) {
        _rightIndex = middleIndex - 1 | 0;
        continue ;
      }
      _leftIndex = middleIndex + 1 | 0;
      continue ;
    };
  }
}

var n1 = [
  -1,
  0,
  3,
  5,
  9,
  12
];

var r1 = binarySearch(n1, 9);

console.log("r1: ", r1);

var n2 = [
  -1,
  0,
  3,
  5,
  9,
  12
];

var r2 = binarySearch(n2, 2);

console.log("r2: ", r2);

var n3 = [5];

var r3 = binarySearch(n3, 5);

console.log("r3: ", r3);

var n4 = [
  -1,
  0,
  3,
  5,
  9,
  12,
  13,
  15,
  17,
  19,
  23,
  29
];

var r4 = binarySearch(n4, 18);

console.log("r4: ", r4);

var t1 = 9;

var t2 = 2;

var t3 = 5;

var t4 = 18;

export {
  binarySearch ,
  n1 ,
  t1 ,
  r1 ,
  n2 ,
  t2 ,
  r2 ,
  n3 ,
  t3 ,
  r3 ,
  n4 ,
  t4 ,
  r4 ,
}
/* r1 Not a pure module */
