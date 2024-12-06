// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Int32 from "rescript/lib/es6/int32.js";
import * as Core__Array from "@rescript/core/src/Core__Array.res.js";

function diffBetweenOnesAndZeroesInRowAndColumn(grid) {
  var maxRows = grid.length;
  var r = grid.at(0);
  var row = r !== undefined ? r : [];
  var maxColumns = row.length;
  var countZeroesAndOnes = function (row) {
    return Core__Array.reduce(row, [
                0,
                0
              ], (function (acc, num) {
                  var onesCount = acc[1];
                  var zeroesCount = acc[0];
                  if (num === 0) {
                    return [
                            zeroesCount + 1 | 0,
                            onesCount
                          ];
                  } else {
                    return [
                            zeroesCount,
                            onesCount + 1 | 0
                          ];
                  }
                }));
  };
  var makeBinaryCountTableRow = function (rowTable, _rowIndex) {
    while(true) {
      var rowIndex = _rowIndex;
      if (rowIndex === maxRows) {
        return rowTable;
      }
      var r = grid.at(rowIndex);
      var row = r !== undefined ? r : [];
      var match = countZeroesAndOnes(row);
      rowTable.set(rowIndex, [
            match[0],
            match[1]
          ]);
      _rowIndex = rowIndex + 1 | 0;
      continue ;
    };
  };
  var makeBinaryCountTableColumn = function (columnTable, _columnIndex) {
    while(true) {
      var columnIndex = _columnIndex;
      if (columnIndex === maxColumns) {
        return columnTable;
      }
      var collectBinaryColumn = (function(columnIndex){
      return function collectBinaryColumn(_binaryColumn, _rowIndex) {
        while(true) {
          var rowIndex = _rowIndex;
          var binaryColumn = _binaryColumn;
          if (rowIndex === maxRows) {
            return binaryColumn;
          }
          var r = grid.at(rowIndex);
          var row = r !== undefined ? r : [];
          var h = row.at(columnIndex);
          var binary = h !== undefined ? h : -1;
          _rowIndex = rowIndex + 1 | 0;
          _binaryColumn = binaryColumn.concat([binary]);
          continue ;
        };
      }
      }(columnIndex));
      var binaryColumn = collectBinaryColumn([], 0);
      var match = countZeroesAndOnes(binaryColumn);
      columnTable.set(columnIndex, [
            match[0],
            match[1]
          ]);
      _columnIndex = columnIndex + 1 | 0;
      continue ;
    };
  };
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
  var rowTable = makeBinaryCountTableRow(new Map(), 0);
  var columnTable = makeBinaryCountTableColumn(new Map(), 0);
  var diffMatrix = makeMatrix(maxRows, maxColumns);
  var _rowIndex = 0;
  while(true) {
    var rowIndex = _rowIndex;
    if (rowIndex === maxRows) {
      return diffMatrix;
    }
    var t = rowTable.get(rowIndex);
    var match = t !== undefined ? t : [
        0,
        0
      ];
    var onesCountRow = match[1];
    var zeroesCountRow = match[0];
    var columnLoop = (function(rowIndex,zeroesCountRow,onesCountRow){
    return function columnLoop(_columnIndex) {
      while(true) {
        var columnIndex = _columnIndex;
        if (columnIndex === maxColumns) {
          return ;
        }
        var t = columnTable.get(columnIndex);
        var match = t !== undefined ? t : [
            0,
            0
          ];
        var diff = ((onesCountRow + match[1] | 0) - zeroesCountRow | 0) - match[0] | 0;
        var row = diffMatrix.at(rowIndex);
        var diffMatrixRow = row !== undefined ? row.map((function(columnIndex,diff){
              return function (num, idx) {
                if (idx === columnIndex) {
                  return diff;
                } else {
                  return num;
                }
              }
              }(columnIndex,diff))) : [];
        diffMatrix[rowIndex] = diffMatrixRow;
        _columnIndex = columnIndex + 1 | 0;
        continue ;
      };
    }
    }(rowIndex,zeroesCountRow,onesCountRow));
    columnLoop(0);
    _rowIndex = rowIndex + 1 | 0;
    continue ;
  };
}

var g1 = [
  [
    0,
    1,
    1
  ],
  [
    1,
    0,
    1
  ],
  [
    0,
    0,
    1
  ]
];

var r1 = diffBetweenOnesAndZeroesInRowAndColumn(g1);

console.log("r1: ", r1);

var g2 = [
  [
    1,
    1,
    1
  ],
  [
    1,
    1,
    1
  ]
];

var r2 = diffBetweenOnesAndZeroesInRowAndColumn(g2);

console.log("r2: ", r2);

export {
  diffBetweenOnesAndZeroesInRowAndColumn ,
  g1 ,
  r1 ,
  g2 ,
  r2 ,
}
/* r1 Not a pure module */
