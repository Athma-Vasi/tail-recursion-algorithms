// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Int32 from "rescript/lib/es6/int32.js";

function slidingWindowMaximum(numbers, windowSize) {
  var length = numbers.length;
  var updateMonoIncrStack = function (_monoIncrStack, numToPush) {
    while(true) {
      var monoIncrStack = _monoIncrStack;
      var stackLength = monoIncrStack.length;
      var num = monoIncrStack.at(-1);
      var prevMaximum = num !== undefined ? num : Int32.min_int;
      if (prevMaximum > numToPush) {
        return monoIncrStack;
      }
      if (monoIncrStack.length < 1) {
        return monoIncrStack.concat([numToPush]);
      }
      _monoIncrStack = monoIncrStack.slice(0, stackLength - 1 | 0);
      continue ;
    };
  };
  var expandWindow = function (_monoIncrStack, _index) {
    while(true) {
      var index = _index;
      var monoIncrStack = _monoIncrStack;
      if (index === windowSize) {
        return monoIncrStack;
      }
      var num = numbers[index];
      var currentNum = num !== undefined ? num : Int32.min_int + 1 | 0;
      var updatedMonoIncrStack = updateMonoIncrStack(monoIncrStack, currentNum);
      _index = index + 1 | 0;
      _monoIncrStack = updatedMonoIncrStack;
      continue ;
    };
  };
  var updatedMonoIncrStack = expandWindow([], 0);
  var num = updatedMonoIncrStack.at(-1);
  var maximum = num !== undefined ? num : Int32.min_int;
  var maximumArrays = [].concat([maximum]);
  var _maximumArrays = maximumArrays;
  var _monoIncrStack = updatedMonoIncrStack;
  var _leftIndex = 1;
  var _rightIndex = windowSize;
  while(true) {
    var rightIndex = _rightIndex;
    var leftIndex = _leftIndex;
    var monoIncrStack = _monoIncrStack;
    var maximumArrays$1 = _maximumArrays;
    if (rightIndex === length) {
      return maximumArrays$1;
    }
    var num$1 = numbers[rightIndex];
    var rightIncludedNum = num$1 !== undefined ? num$1 : Int32.min_int;
    var rightUpdatedMonoIncrStack = updateMonoIncrStack(monoIncrStack, rightIncludedNum);
    var num$2 = rightUpdatedMonoIncrStack.at(-1);
    var maximum$1 = num$2 !== undefined ? num$2 : Int32.min_int;
    _rightIndex = rightIndex + 1 | 0;
    _leftIndex = leftIndex + 1 | 0;
    _monoIncrStack = rightUpdatedMonoIncrStack;
    _maximumArrays = maximumArrays$1.concat([maximum$1]);
    continue ;
  };
}

console.log(slidingWindowMaximum([
          1,
          3,
          -1,
          -3,
          5,
          3,
          6,
          7
        ], 3));

console.log(slidingWindowMaximum([1], 1));

export {
  slidingWindowMaximum ,
}
/*  Not a pure module */
