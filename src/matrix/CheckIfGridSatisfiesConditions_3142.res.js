// Generated by ReScript, PLEASE EDIT WITH CARE


function checkIfGridSatisfiesConditions(grid) {
  var checkColumnWiseInequalities = function (inequalities, _yIndex) {
    while(true) {
      var yIndex = _yIndex;
      if (yIndex === grid.length) {
        return inequalities;
      }
      var arr = grid.at(yIndex);
      var row = arr !== undefined ? arr : [];
      var checkColumn = (function(row){
      return function checkColumn(columnInequalities, _stack, _xIndex) {
        while(true) {
          var xIndex = _xIndex;
          var stack = _stack;
          if (xIndex === row.length) {
            return columnInequalities;
          }
          var n = row.at(xIndex);
          var currNum = n !== undefined ? n : -1;
          var n$1 = stack.at(-1);
          var prevNum = n$1 !== undefined ? n$1 : -1;
          var updatedStack = stack.concat([currNum]);
          if (prevNum < 0) {
            _xIndex = xIndex + 1 | 0;
            _stack = updatedStack;
            continue ;
          }
          columnInequalities.add(prevNum !== currNum);
          _xIndex = xIndex + 1 | 0;
          _stack = updatedStack;
          continue ;
        };
      }
      }(row));
      var columnInequalities;
      if (row.length < 2) {
        var columnInequalities$1 = new Set();
        columnInequalities$1.add(false);
        columnInequalities = columnInequalities$1;
      } else {
        columnInequalities = checkColumn(new Set(), [], 0);
      }
      inequalities.add(columnInequalities.has(false) ? false : true);
      _yIndex = yIndex + 1 | 0;
      continue ;
    };
  };
  var checkRowEqualities = function (equalities, _yIndex) {
    while(true) {
      var yIndex = _yIndex;
      if (grid.length < 2) {
        var equalities$1 = new Set();
        equalities$1.add(false);
        return equalities$1;
      }
      if (yIndex === grid.length) {
        return equalities;
      }
      var arr = grid.at(yIndex);
      var row = arr !== undefined ? arr : [];
      var checkRow = (function(row){
      return function checkRow(rowEqualities, _stack, _xIndex) {
        while(true) {
          var xIndex = _xIndex;
          var stack = _stack;
          if (xIndex === row.length) {
            return equalities;
          }
          var n = row.at(xIndex);
          var currNum = n !== undefined ? n : -1;
          var arr = stack.at(-1);
          var prevRow = arr !== undefined ? arr : [];
          var updatedStack = stack.concat([row]);
          if (prevRow.length === 0) {
            _xIndex = xIndex + 1 | 0;
            _stack = updatedStack;
            continue ;
          }
          var n$1 = prevRow.at(xIndex);
          var prevNum = n$1 !== undefined ? n$1 : -1;
          rowEqualities.add(prevNum === currNum);
          _xIndex = xIndex + 1 | 0;
          _stack = updatedStack;
          continue ;
        };
      }
      }(row));
      var rowEqualities = checkRow(new Set(), [], 0);
      equalities.add(rowEqualities.has(false) ? false : true);
      _yIndex = yIndex + 1 | 0;
      continue ;
    };
  };
  var columnInequality = checkColumnWiseInequalities(new Set(), 0).has(false);
  var rowEquality = checkRowEqualities(new Set(), 0).has(false);
  if (columnInequality) {
    return false;
  } else {
    return !rowEquality;
  }
}

var g1 = [
  [
    1,
    0,
    2
  ],
  [
    1,
    0,
    2
  ]
];

var r1 = checkIfGridSatisfiesConditions(g1);

console.log("r1: ", r1);

var g2 = [
  [
    1,
    1,
    1
  ],
  [
    0,
    0,
    0
  ]
];

var r2 = checkIfGridSatisfiesConditions(g2);

console.log("r2: ", r2);

var g3 = [
  [1],
  [2],
  [3]
];

var r3 = checkIfGridSatisfiesConditions(g3);

console.log("r3: ", r3);

export {
  checkIfGridSatisfiesConditions ,
  g1 ,
  r1 ,
  g2 ,
  r2 ,
  g3 ,
  r3 ,
}
/* r1 Not a pure module */