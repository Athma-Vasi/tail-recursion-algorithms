// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Core__Array from "@rescript/core/src/Core__Array.res.js";

function anagrams(str, subString) {
  var stringLength = str.length;
  var makeSubStringTable = function (subString) {
    return Core__Array.reduce(subString.split(""), new Map(), (function (acc, $$char) {
                  var count = acc.get($$char);
                  var existingCount = count !== undefined ? count : 0;
                  acc.set($$char, existingCount + 1 | 0);
                  return acc;
                }));
  };
  var updateTable = function (subStringTable, _counter, index) {
    while(true) {
      var counter = _counter;
      if (counter > subStringTable.size) {
        return subStringTable;
      }
      var c = str[counter + index | 0];
      var $$char = c !== undefined ? c : "";
      console.log("--updateTable--");
      console.log("char", $$char);
      if (subStringTable.has($$char)) {
        var count = subStringTable.get($$char);
        var existingCount = count !== undefined ? count : 0;
        subStringTable.set($$char, existingCount - 1 | 0);
      }
      console.log("subStringTable", subStringTable);
      _counter = counter + 1 | 0;
      continue ;
    };
  };
  var initialSubStringTable = makeSubStringTable(subString);
  var updatedSubStringTable = updateTable(initialSubStringTable, 1, -1);
  console.log("--before loop--");
  console.log("updatedSubStringTable", updatedSubStringTable);
  var _startIndices = [];
  var _subStringTable = updatedSubStringTable;
  var _leftIndex = 0;
  var _rightIndex = subString.length - 1 | 0;
  while(true) {
    var rightIndex = _rightIndex;
    var leftIndex = _leftIndex;
    var subStringTable = _subStringTable;
    var startIndices = _startIndices;
    var counts = Core__Array.reduce(Array.from(subStringTable.entries()), 0, (function (acc, param) {
            return acc + param[1] | 0;
          }));
    if (rightIndex === stringLength) {
      if (counts === 0) {
        return startIndices.concat([leftIndex]);
      } else {
        return startIndices;
      }
    }
    if (counts === 0) {
      var initialSubStringTable$1 = makeSubStringTable(subString);
      var updatedSubStringTable$1 = updateTable(initialSubStringTable$1, 1, leftIndex);
      console.log("--loop--");
      console.log("leftIndex", leftIndex);
      console.log("rightIndex", rightIndex);
      console.log("startIndices", startIndices);
      console.log("updatedSubStringTable", updatedSubStringTable$1);
      var $$char = str[leftIndex];
      var leftChar = $$char !== undefined ? $$char : "";
      if (updatedSubStringTable$1.has(leftChar)) {
        var count = updatedSubStringTable$1.get(leftChar);
        var existingCount = count !== undefined ? count : 0;
        updatedSubStringTable$1.set(leftChar, existingCount + 1 | 0);
      }
      var $$char$1 = str[rightIndex + 1 | 0];
      var rightChar = $$char$1 !== undefined ? $$char$1 : "";
      if (updatedSubStringTable$1.has(rightChar)) {
        var count$1 = updatedSubStringTable$1.get(rightChar);
        var existingCount$1 = count$1 !== undefined ? count$1 : 0;
        updatedSubStringTable$1.set(rightChar, existingCount$1 - 1 | 0);
      }
      console.log("leftChar", leftChar);
      console.log("rightChar", rightChar);
      console.log("updatedSubStringTable", updatedSubStringTable$1);
      _rightIndex = rightIndex + 1 | 0;
      _leftIndex = leftIndex + 1 | 0;
      _subStringTable = updatedSubStringTable$1;
      _startIndices = startIndices.concat([leftIndex]);
      continue ;
    }
    var $$char$2 = str[leftIndex];
    var leftChar$1 = $$char$2 !== undefined ? $$char$2 : "";
    if (subStringTable.has(leftChar$1)) {
      var count$2 = subStringTable.get(leftChar$1);
      var existingCount$2 = count$2 !== undefined ? count$2 : 0;
      subStringTable.set(leftChar$1, existingCount$2 + 1 | 0);
    }
    var $$char$3 = str[rightIndex + 1 | 0];
    var rightChar$1 = $$char$3 !== undefined ? $$char$3 : "";
    if (subStringTable.has(rightChar$1)) {
      var count$3 = subStringTable.get(rightChar$1);
      var existingCount$3 = count$3 !== undefined ? count$3 : 0;
      subStringTable.set(rightChar$1, existingCount$3 - 1 | 0);
    }
    console.log("--valid anagram does not exist--");
    console.log("leftChar", leftChar$1);
    console.log("rightChar", rightChar$1);
    console.log("subStringTable", subStringTable);
    _rightIndex = rightIndex + 1 | 0;
    _leftIndex = leftIndex + 1 | 0;
    continue ;
  };
}

var s1 = "cbaebabacd";

var p1 = "abc";

var r1 = anagrams(s1, p1);

console.log("cbaebabacd   abc", r1);

export {
  anagrams ,
  s1 ,
  p1 ,
  r1 ,
}
/* r1 Not a pure module */
