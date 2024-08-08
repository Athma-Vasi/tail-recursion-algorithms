// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Core__Array from "@rescript/core/src/Core__Array.res.js";

function productOfArrayExceptSelf(nums) {
  var prefixProducts = Core__Array.reduceWithIndex(nums, [1], (function (acc, _curr, idx) {
          if (idx < 1) {
            return acc;
          }
          var n = nums.at(idx - 1 | 0);
          var num = n !== undefined ? n : 0;
          var n$1 = acc.at(-1);
          var prev = n$1 !== undefined ? n$1 : 0;
          return acc.concat([Math.imul(num, prev)]);
        }));
  var suffixProducts = Core__Array.reduceRightWithIndex(nums, [1], (function (acc, _curr, idx) {
          if (idx > (nums.length - 2 | 0)) {
            return acc;
          }
          var n = nums.at(idx + 1 | 0);
          var num = n !== undefined ? n : 0;
          var n$1 = acc.at(0);
          var prev = n$1 !== undefined ? n$1 : 0;
          return [Math.imul(num, prev)].concat(acc);
        }));
  return Core__Array.reduceWithIndex(prefixProducts, [], (function (result, prefixProduct, idx) {
                var n = suffixProducts.at(idx);
                var suffixProduct = n !== undefined ? n : 0;
                return result.concat([Math.imul(prefixProduct, suffixProduct)]);
              }));
}

var n1 = [
  2,
  3,
  4,
  5
];

var r1 = productOfArrayExceptSelf(n1);

console.log("r1: ", r1);

var n2 = [
  1,
  2,
  3,
  4
];

var r2 = productOfArrayExceptSelf(n2);

console.log("r2: ", r2);

var n3 = [
  -1,
  1,
  0,
  -3,
  3
];

var r3 = productOfArrayExceptSelf(n3);

console.log("r3: ", r3);

export {
  productOfArrayExceptSelf ,
  n1 ,
  r1 ,
  n2 ,
  r2 ,
  n3 ,
  r3 ,
}
/* r1 Not a pure module */