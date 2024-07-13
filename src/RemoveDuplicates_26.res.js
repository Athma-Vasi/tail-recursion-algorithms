// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Int32 from "rescript/lib/es6/int32.js";

function removeDuplicates(nums) {
  var length = nums.length;
  var _leftIndex = 0;
  var _rightIndex = 1;
  while(true) {
    var rightIndex = _rightIndex;
    var leftIndex = _leftIndex;
    var num = nums[leftIndex];
    var leftNum = num !== undefined ? num : Int32.min_int;
    var num$1 = nums[rightIndex];
    var rightNum = num$1 !== undefined ? num$1 : Int32.min_int;
    if (rightIndex === length) {
      return nums;
    }
    if (leftNum === rightNum) {
      _rightIndex = rightIndex + 1 | 0;
      continue ;
    }
    nums[leftIndex + 1 | 0] = rightNum;
    _rightIndex = rightIndex + 1 | 0;
    _leftIndex = leftIndex + 1 | 0;
    continue ;
  };
}

var n1 = [
  1,
  1,
  2
];

var r1 = removeDuplicates(n1);

console.log("[1,1,2]", r1);

var n2 = [
  0,
  0,
  1,
  1,
  1,
  2,
  2,
  3,
  3,
  4
];

var r2 = removeDuplicates(n2);

console.log("[0,0,1,1,1,2,2,3,3,4]", n2);

export {
  removeDuplicates ,
  n1 ,
  r1 ,
  n2 ,
  r2 ,
}
/* r1 Not a pure module */
