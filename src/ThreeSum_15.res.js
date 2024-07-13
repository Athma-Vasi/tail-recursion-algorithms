// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Int32 from "rescript/lib/es6/int32.js";

function threeSum(nums) {
  var length = nums.length;
  var clone = nums.map(function (num) {
        return num;
      });
  clone.sort(function (a, b) {
        return a - b | 0;
      });
  var triplets = new Set();
  var _anchorIndex = 0;
  while(true) {
    var anchorIndex = _anchorIndex;
    var num = clone[anchorIndex - 1 | 0];
    var prevAnchor = num !== undefined ? num : Int32.min_int;
    var num$1 = clone[anchorIndex];
    var currentAnchor = num$1 !== undefined ? num$1 : Int32.min_int;
    var explorersLoop = (function(currentAnchor){
    return function explorersLoop(_lowIndex, _highIndex) {
      while(true) {
        var highIndex = _highIndex;
        var lowIndex = _lowIndex;
        var num = clone[lowIndex];
        var lowExplorer = num !== undefined ? num : Int32.min_int;
        var num$1 = clone[highIndex];
        var highExplorer = num$1 !== undefined ? num$1 : Int32.min_int;
        var sum = (currentAnchor + lowExplorer | 0) + highExplorer | 0;
        if (lowIndex === highIndex || lowIndex === length || highIndex === 0 || lowIndex > highIndex) {
          return ;
        }
        if (sum === 0) {
          triplets.add([
                currentAnchor,
                lowExplorer,
                highExplorer
              ]);
          _highIndex = highIndex - 1 | 0;
          _lowIndex = lowIndex + 1 | 0;
          continue ;
        }
        if (sum < 0) {
          _lowIndex = lowIndex + 1 | 0;
          continue ;
        }
        _highIndex = highIndex - 1 | 0;
        continue ;
      };
    }
    }(currentAnchor));
    if (prevAnchor === currentAnchor) {
      
    } else {
      explorersLoop(anchorIndex + 1 | 0, length - 1 | 0);
    }
    if (anchorIndex === (length - 2 | 0)) {
      return Array.from(triplets.values());
    }
    _anchorIndex = anchorIndex + 1 | 0;
    continue ;
  };
}

var n1 = [
  -1,
  0,
  1,
  2,
  -1,
  -4
];

var r1 = threeSum(n1);

console.log("[-1,0,1,2,-1,-4]", r1);

export {
  threeSum ,
  n1 ,
  r1 ,
}
/* r1 Not a pure module */