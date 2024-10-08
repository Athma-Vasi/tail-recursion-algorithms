// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Int32 from "rescript/lib/es6/int32.js";
import * as Core__Array from "@rescript/core/src/Core__Array.res.js";

function findMiddleIndex(nums) {
  var length = nums.length;
  var total = Core__Array.reduce(nums, 0, (function (acc, num) {
          return acc + num | 0;
        }));
  var _index = 0;
  var _leftSum = 0;
  while(true) {
    var leftSum = _leftSum;
    var index = _index;
    var num = nums.at(index);
    var currentNum = num !== undefined ? num : Int32.max_int;
    var newLeftSum = leftSum + currentNum | 0;
    var rightSum = total - newLeftSum | 0;
    if (leftSum === rightSum) {
      return index;
    }
    if (index === (length - 1 | 0)) {
      return -1;
    }
    _leftSum = newLeftSum;
    _index = index + 1 | 0;
    continue ;
  };
}

var n1 = [
  2,
  3,
  -1,
  8,
  4
];

var r1 = findMiddleIndex(n1);

console.log("[2,3,-1,8,4]", r1);

var n2 = [
  1,
  -1,
  4
];

var r2 = findMiddleIndex(n2);

console.log("[1,-1,4]", r2);

var n3 = [
  2,
  5
];

var r3 = findMiddleIndex(n3);

console.log("[2,5]", r3);

export {
  findMiddleIndex ,
  n1 ,
  r1 ,
  n2 ,
  r2 ,
  n3 ,
  r3 ,
}
/* r1 Not a pure module */
