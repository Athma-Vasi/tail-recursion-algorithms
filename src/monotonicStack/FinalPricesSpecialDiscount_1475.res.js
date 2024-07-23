// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Int32 from "rescript/lib/es6/int32.js";
import * as Core__Array from "@rescript/core/src/Core__Array.res.js";

function finalPricesSpecialDiscount(prices) {
  var length = prices.length;
  var updateMonoIncrStack = function (discounts, indexToPush, _monoIncrStack) {
    while(true) {
      var monoIncrStack = _monoIncrStack;
      var stackLength = monoIncrStack.length;
      var num = prices[indexToPush];
      var numToPush = num !== undefined ? num : Int32.min_int;
      var num$1 = monoIncrStack.at(-1);
      var prevIndex = num$1 !== undefined ? num$1 : Int32.min_int;
      var num$2 = prices[prevIndex];
      var prevMaximum = num$2 !== undefined ? num$2 : Int32.min_int + 1 | 0;
      if (numToPush > prevMaximum || stackLength < 1) {
        return [
                discounts,
                monoIncrStack.concat([indexToPush])
              ];
      }
      discounts[prevIndex] = prevMaximum - numToPush | 0;
      _monoIncrStack = monoIncrStack.slice(0, stackLength - 1 | 0);
      continue ;
    };
  };
  var loop = function (_discounts, _index, _monoIncrStack) {
    while(true) {
      var monoIncrStack = _monoIncrStack;
      var index = _index;
      var discounts = _discounts;
      if (index === length) {
        return [
                discounts,
                monoIncrStack
              ];
      }
      var match = updateMonoIncrStack(discounts, index, monoIncrStack);
      _monoIncrStack = match[1];
      _index = index + 1 | 0;
      _discounts = match[0];
      continue ;
    };
  };
  var match = loop(Core__Array.make(length, -1), 0, []);
  var monoIncrStack = match[1];
  var discounts = match[0];
  if (monoIncrStack.length > 0) {
    return Core__Array.reduceRight(monoIncrStack, discounts, (function (acc, currIndex) {
                  var num = prices[currIndex];
                  var price = num !== undefined ? num : Int32.min_int;
                  acc[currIndex] = price;
                  return acc;
                }));
  } else {
    return discounts;
  }
}

var p1 = [
  8,
  4,
  6,
  2,
  3
];

var r1 = finalPricesSpecialDiscount(p1);

console.log("r1", r1);

var p2 = [
  1,
  2,
  3,
  4,
  5
];

var r2 = finalPricesSpecialDiscount(p2);

console.log("r2", r2);

var p3 = [
  10,
  1,
  1,
  6
];

var r3 = finalPricesSpecialDiscount(p3);

console.log("r3", r3);

export {
  finalPricesSpecialDiscount ,
  p1 ,
  r1 ,
  p2 ,
  r2 ,
  p3 ,
  r3 ,
}
/* r1 Not a pure module */
