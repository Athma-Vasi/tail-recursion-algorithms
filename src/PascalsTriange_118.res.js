// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Int32 from "rescript/lib/es6/int32.js";
import * as Core__Array from "@rescript/core/src/Core__Array.res.js";

function pascalsTriangle(numRows) {
  var initialTriangle = [
    [1],
    [
      1,
      1
    ]
  ];
  var _accumulator = initialTriangle;
  var _height = 2;
  while(true) {
    var height = _height;
    var accumulator = _accumulator;
    var row = accumulator[height - 1 | 0];
    var prevRow = row !== undefined ? row : [];
    var length = prevRow.length;
    var rowLoop = (function(prevRow,length){
    return function rowLoop(_currentRow, _index) {
      while(true) {
        var index = _index;
        var currentRow = _currentRow;
        var num = prevRow[index];
        var leftNum = num !== undefined ? num : Int32.min_int;
        var num$1 = prevRow[index + 1 | 0];
        var rightNum = num$1 !== undefined ? num$1 : Int32.min_int;
        var sum = leftNum + rightNum | 0;
        var rowClone = currentRow.map((function(index,sum){
            return function (num, idx) {
              if (idx === (index + 1 | 0)) {
                return sum;
              } else {
                return num;
              }
            }
            }(index,sum)));
        if (index === (length - 1 | 0)) {
          return currentRow;
        }
        _index = index + 1 | 0;
        _currentRow = rowClone;
        continue ;
      };
    }
    }(prevRow,length));
    var currentRow = Core__Array.make(height + 1 | 0, 1);
    var newRow = rowLoop(currentRow, 0);
    var accClone = accumulator.map(function (num) {
          return num;
        });
    accClone.push(newRow);
    if (height === (numRows - 1 | 0)) {
      return accClone;
    }
    _height = height + 1 | 0;
    _accumulator = accClone;
    continue ;
  };
}

var result1 = pascalsTriangle(5);

console.log("5", result1);

var numRows1 = 5;

export {
  pascalsTriangle ,
  numRows1 ,
  result1 ,
}
/* result1 Not a pure module */
