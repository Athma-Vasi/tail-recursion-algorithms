// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Core__Array from "@rescript/core/src/Core__Array.res.js";
import * as Core__Option from "@rescript/core/src/Core__Option.res.js";

function applyOperationsToAnArray(nums) {
  var length = nums.length;
  var operation = function (_modified, _leftIndex) {
    while(true) {
      var leftIndex = _leftIndex;
      var modified = _modified;
      if (leftIndex === (length - 1 | 0)) {
        return modified;
      }
      var leftNum = Core__Option.mapOr(modified.at(leftIndex), -1, (function (n) {
              return n;
            }));
      var rightNum = Core__Option.mapOr(modified.at(leftIndex + 1 | 0), -1, (function (n) {
              return n;
            }));
      if (leftNum === rightNum) {
        var newModified = modified.map((function(leftIndex,leftNum){
              return function (n, index) {
                if (index === leftIndex) {
                  return (leftNum << 1);
                } else {
                  return n;
                }
              }
              }(leftIndex,leftNum))).map((function(leftIndex){
            return function (n, index) {
              if (index === (leftIndex + 1 | 0)) {
                return 0;
              } else {
                return n;
              }
            }
            }(leftIndex)));
        _leftIndex = leftIndex + 1 | 0;
        _modified = newModified;
        continue ;
      }
      _leftIndex = leftIndex + 1 | 0;
      continue ;
    };
  };
  var match = Core__Array.reduce(operation(nums, 0), [
        [],
        []
      ], (function (acc, num) {
          var zeroes = acc[1];
          var nonZeroes = acc[0];
          if (num > 0) {
            return [
                    nonZeroes.concat([num]),
                    zeroes
                  ];
          } else {
            return [
                    nonZeroes,
                    zeroes.concat([num])
                  ];
          }
        }));
  return match[0].concat(match[1]);
}

var n1 = [
  1,
  2,
  2,
  1,
  1,
  0
];

var r1 = applyOperationsToAnArray(n1);

console.log("r1: ", r1);

var n2 = [
  0,
  1
];

var r2 = applyOperationsToAnArray(n2);

console.log("r2: ", r2);

export {
  applyOperationsToAnArray ,
  n1 ,
  r1 ,
  n2 ,
  r2 ,
}
/* r1 Not a pure module */
