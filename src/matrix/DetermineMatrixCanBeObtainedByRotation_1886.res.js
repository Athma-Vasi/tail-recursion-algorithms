// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Int32 from "rescript/lib/es6/int32.js";
import * as Core__Array from "@rescript/core/src/Core__Array.res.js";

function determineMatrixCanBeObtainedByRotation(matrix, target) {
  var numberOfRows = matrix.length;
  var arr = matrix.at(0);
  var row = arr !== undefined ? arr : [];
  var numberOfColumns = row.length;
  var areMatrixesEqual = function (matrix1, matrix2) {
    var rowCoveredSet = new Set();
    var _rowIndex = 0;
    while(true) {
      var rowIndex = _rowIndex;
      if (rowIndex === numberOfRows) {
        return rowCoveredSet.has(false);
      }
      var arr = matrix1.at(rowIndex);
      var row1 = arr !== undefined ? arr : [];
      var arr$1 = matrix2.at(rowIndex);
      var row2 = arr$1 !== undefined ? arr$1 : [];
      var columnLoop = function (colCoveredSet, _columnIndex, row1, row2) {
        while(true) {
          var columnIndex = _columnIndex;
          if (columnIndex === numberOfColumns) {
            return colCoveredSet.has(false);
          }
          var num = row1.at(columnIndex);
          var num1 = num !== undefined ? num : Int32.min_int;
          var num$1 = row2.at(columnIndex);
          var num2 = num$1 !== undefined ? num$1 : Int32.min_int;
          colCoveredSet.add(num1 === num2);
          _columnIndex = columnIndex + 1 | 0;
          continue ;
        };
      };
      var areColumnsEqual = columnLoop(new Set(), 0, row1, row2);
      rowCoveredSet.add(areColumnsEqual);
      _rowIndex = rowIndex + 1 | 0;
      continue ;
    };
  };
  var transposeMatrix = function (matrix) {
    var makeMatrix = function (numberOfRows, numberOfColumns) {
      var _matrix = [];
      var _rowCounter = 0;
      while(true) {
        var rowCounter = _rowCounter;
        var matrix = _matrix;
        if (rowCounter === numberOfRows) {
          return matrix;
        }
        _rowCounter = rowCounter + 1 | 0;
        _matrix = matrix.concat([Core__Array.make(numberOfColumns, Int32.min_int)]);
        continue ;
      };
    };
    var _transposedMatrix = makeMatrix(numberOfRows, numberOfColumns);
    var _rowIndex = 0;
    while(true) {
      var rowIndex = _rowIndex;
      var transposedMatrix = _transposedMatrix;
      if (rowIndex === numberOfRows) {
        return transposedMatrix;
      }
      var arr = matrix.at(rowIndex);
      var row = arr !== undefined ? arr : [];
      var columnLoop = (function(transposedMatrix,rowIndex,row){
      return function columnLoop(_transposedRow, _columnIndex) {
        while(true) {
          var columnIndex = _columnIndex;
          var transposedRow = _transposedRow;
          if (columnIndex === numberOfColumns) {
            return transposedRow;
          }
          var num = row.at(columnIndex);
          var matrixNum = num !== undefined ? num : Int32.min_int;
          var num$1 = transposedMatrix.at(columnIndex);
          var transposedRow$1 = num$1 !== undefined ? num$1 : [];
          var updatedRow = transposedRow$1.map((function(matrixNum){
              return function (num, idx) {
                if (idx === rowIndex) {
                  return matrixNum;
                } else {
                  return num;
                }
              }
              }(matrixNum)));
          _columnIndex = columnIndex + 1 | 0;
          _transposedRow = updatedRow;
          continue ;
        };
      }
      }(transposedMatrix,rowIndex,row));
      var transposedRow = columnLoop(Core__Array.make(numberOfColumns, Int32.min_int), 0);
      var updatedTransposedMatrix = transposedMatrix.map((function(rowIndex,transposedRow){
          return function (row, idx) {
            if (idx === rowIndex) {
              return transposedRow;
            } else {
              return row;
            }
          }
          }(rowIndex,transposedRow)));
      _rowIndex = rowIndex + 1 | 0;
      _transposedMatrix = updatedTransposedMatrix;
      continue ;
    };
  };
  var reverseRows = function (matrix) {
    var _reversed = [];
    var _index = 0;
    while(true) {
      var index = _index;
      var reversed = _reversed;
      if (index === matrix.length) {
        return reversed;
      }
      var arr = matrix.at(index);
      var row = arr !== undefined ? arr : [];
      _index = index + 1 | 0;
      _reversed = reversed.concat([row.toReversed()]);
      continue ;
    };
  };
  var isObtainedSet = new Set();
  var _counter = 0;
  var rotations = 4;
  while(true) {
    var counter = _counter;
    if (counter === rotations) {
      return isObtainedSet.has(true);
    }
    var transposedMatrix = transposeMatrix(matrix);
    var rotatedMatrix = reverseRows(transposedMatrix);
    isObtainedSet.add(areMatrixesEqual(rotatedMatrix, target));
    _counter = counter + 1 | 0;
    continue ;
  };
}

var m1 = [
  [
    0,
    1
  ],
  [
    1,
    0
  ]
];

var t1 = [
  [
    1,
    0
  ],
  [
    0,
    1
  ]
];

var r1 = determineMatrixCanBeObtainedByRotation(m1, t1);

console.log("r1: ", r1);

var m2 = [
  [
    0,
    1
  ],
  [
    1,
    1
  ]
];

var t2 = [
  [
    1,
    0
  ],
  [
    0,
    1
  ]
];

var r2 = determineMatrixCanBeObtainedByRotation(m2, t2);

console.log("r2: ", r2);

var m3 = [
  [
    0,
    0,
    0
  ],
  [
    0,
    1,
    0
  ],
  [
    1,
    1,
    1
  ]
];

var t3 = [
  [
    1,
    1,
    1
  ],
  [
    0,
    1,
    0
  ],
  [
    0,
    0,
    0
  ]
];

var r3 = determineMatrixCanBeObtainedByRotation(m3, t3);

console.log("r3: ", r3);

export {
  determineMatrixCanBeObtainedByRotation ,
  m1 ,
  t1 ,
  r1 ,
  m2 ,
  t2 ,
  r2 ,
  m3 ,
  t3 ,
  r3 ,
}
/* r1 Not a pure module */
