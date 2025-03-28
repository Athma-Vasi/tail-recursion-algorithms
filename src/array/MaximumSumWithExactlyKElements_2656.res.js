// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Int32 from "rescript/lib/es6/int32.js";
import * as Core__Array from "@rescript/core/src/Core__Array.res.js";

function maximumSumWithExactlyKElements(nums, k) {
  var max = Core__Array.reduce(nums, Int32.min_int, (function (acc, num) {
          if (acc > num) {
            return acc;
          } else {
            return num;
          }
        }));
  return Math.imul(max, k) + (Math.imul(k, k - 1 | 0) / 2 | 0) | 0;
}

var n1 = [
  1,
  2,
  3,
  4,
  5
];

var r1 = maximumSumWithExactlyKElements(n1, 3);

console.log("r1: ", r1);

var n2 = [
  5,
  5,
  5
];

var r2 = maximumSumWithExactlyKElements(n2, 2);

console.log("r2: ", r2);

var k1 = 3;

var k2 = 2;

export {
  maximumSumWithExactlyKElements ,
  n1 ,
  k1 ,
  r1 ,
  n2 ,
  k2 ,
  r2 ,
}
/* r1 Not a pure module */
