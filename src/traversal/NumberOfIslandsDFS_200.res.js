// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Core__Option from "@rescript/core/src/Core__Option.res.js";

function numberOfIslandsDFS(grid) {
  var maxRows = grid.length;
  var row = Core__Option.getOr(Core__Option.map(grid.at(0), (function (r) {
              return r;
            })), []);
  var maxColumns = row.length;
  var depthFirstSearch = function (visited, _direction, _start, _current) {
    while(true) {
      var current = _current;
      var start = _start;
      var direction = _direction;
      var startColumnIndex = start[1];
      var startRowIndex = start[0];
      var startCell = Core__Option.getOr(Core__Option.flatMap(grid.at(startRowIndex), (function(startColumnIndex){
              return function (r) {
                return r.at(startColumnIndex);
              }
              }(startColumnIndex))), 0);
      var nextRowIndex = (startRowIndex + 1 | 0) > maxRows ? -1 : startRowIndex + 1 | 0;
      var nextColumnIndex = (startColumnIndex + 1 | 0) > maxColumns ? -1 : startColumnIndex + 1 | 0;
      var nextRowCell = Core__Option.getOr(Core__Option.flatMap(grid.at(nextRowIndex), (function (r) {
                  return r.at(0);
                })), 1);
      var nextColumnCell = Core__Option.getOr(Core__Option.flatMap(grid.at(startRowIndex), (function(nextColumnIndex){
              return function (r) {
                return r.at(nextColumnIndex);
              }
              }(nextColumnIndex))), 1);
      var currColumnIndex = current[1];
      var currRowIndex = current[0];
      var currCell = Core__Option.getOr(Core__Option.flatMap(grid.at(currRowIndex), (function(currColumnIndex){
              return function (row) {
                return row.at(currColumnIndex);
              }
              }(currColumnIndex))), 0);
      visited.add(currRowIndex.toString() + "," + currColumnIndex.toString());
      if (startCell === 0 || nextRowCell === 0 && nextColumnCell === 0) {
        return visited;
      }
      switch (direction) {
        case "Up" :
            if (currRowIndex < 0 || currCell === 0) {
              _current = start;
              _direction = "Right";
              continue ;
            }
            _current = [
              currRowIndex - 1 | 0,
              currColumnIndex
            ];
            continue ;
        case "Right" :
            if (currColumnIndex > maxColumns || currCell === 0) {
              _current = start;
              _direction = "Down";
              continue ;
            }
            _current = [
              currRowIndex,
              currColumnIndex + 1 | 0
            ];
            continue ;
        case "Down" :
            if (currRowIndex > maxRows || currCell === 0) {
              _current = start;
              _direction = "Left";
              continue ;
            }
            _current = [
              currRowIndex + 1 | 0,
              currColumnIndex
            ];
            continue ;
        case "Left" :
            if (currColumnIndex < 0 || currCell === 0) {
              var newStart_0 = (startColumnIndex + 1 | 0) > maxColumns ? startRowIndex + 1 | 0 : startRowIndex;
              var newStart_1 = startColumnIndex + 1 | 0;
              var newStart = [
                newStart_0,
                newStart_1
              ];
              _current = newStart;
              _start = newStart;
              _direction = "Right";
              continue ;
            }
            _current = [
              currRowIndex,
              currColumnIndex - 1 | 0
            ];
            continue ;
        
      }
    };
  };
  var _count = 0;
  var _rowVisited = new Set();
  var _rowIndex = 0;
  while(true) {
    var rowIndex = _rowIndex;
    var rowVisited = _rowVisited;
    var count = _count;
    if (rowIndex === maxRows) {
      return count;
    }
    var row$1 = Core__Option.getOr(Core__Option.map(grid.at(rowIndex), (function (r) {
                return r;
              })), []);
    var columnLoop = (function(rowIndex,row$1){
    return function columnLoop(_columnCount, _columnVisited, _columnIndex) {
      while(true) {
        var columnIndex = _columnIndex;
        var columnVisited = _columnVisited;
        var columnCount = _columnCount;
        if (columnIndex === maxColumns) {
          return [
                  columnCount,
                  columnVisited
                ];
        }
        var cell = Core__Option.getOr(Core__Option.map(row$1.at(columnIndex), (function (c) {
                    return c;
                  })), 0);
        var start = [
          rowIndex,
          columnIndex
        ];
        var isCellVisited = columnVisited.has(rowIndex.toString() + "," + columnIndex.toString());
        if (cell === 0 || isCellVisited) {
          _columnIndex = columnIndex + 1 | 0;
          continue ;
        }
        var columnVisited_ = depthFirstSearch(columnVisited, "Up", start, start);
        _columnIndex = columnIndex + 1 | 0;
        _columnVisited = columnVisited_;
        _columnCount = columnCount + 1 | 0;
        continue ;
      };
    }
    }(rowIndex,row$1));
    var match = columnLoop(count, rowVisited, 0);
    _rowIndex = rowIndex + 1 | 0;
    _rowVisited = match[1];
    _count = match[0];
    continue ;
  };
}

var g1 = [
  [
    1,
    1,
    1,
    1,
    0
  ],
  [
    1,
    1,
    0,
    1,
    0
  ],
  [
    1,
    1,
    0,
    0,
    0
  ],
  [
    0,
    0,
    0,
    0,
    0
  ]
];

var r1 = numberOfIslandsDFS(g1);

console.log("r1: ", r1);

var g2 = [
  [
    1,
    1,
    0,
    0,
    0
  ],
  [
    1,
    1,
    0,
    0,
    0
  ],
  [
    0,
    0,
    1,
    0,
    0
  ],
  [
    0,
    0,
    0,
    1,
    1
  ]
];

var r2 = numberOfIslandsDFS(g2);

console.log("r2: ", r2);

export {
  numberOfIslandsDFS ,
  g1 ,
  r1 ,
  g2 ,
  r2 ,
}
/* r1 Not a pure module */