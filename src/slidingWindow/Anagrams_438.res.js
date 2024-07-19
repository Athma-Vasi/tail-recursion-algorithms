// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Core__Array from "@rescript/core/src/Core__Array.res.js";

function anagrams(str, subString) {
  var stringLength = str.length;
  var makeSubStringTable = function (subString) {
    return Core__Array.reduce(subString.split(""), new Map(), (function (acc, $$char) {
                  var $$char$1 = acc.get($$char);
                  var existingCount = $$char$1 !== undefined ? $$char$1 : 0;
                  acc.set($$char, existingCount + 1 | 0);
                  return acc;
                }));
  };
  var expandWindow = function (subStringTable, lowIndex, newLowIndex, _highIndex) {
    while(true) {
      var highIndex = _highIndex;
      if (((highIndex - lowIndex | 0) + 1 | 0) === subStringTable.size) {
        return [
                subStringTable,
                highIndex
              ];
      }
      var $$char = str[newLowIndex];
      var highChar = $$char !== undefined ? $$char : "";
      if (subStringTable.has(highChar)) {
        var count = subStringTable.get(highChar);
        var existingCount = count !== undefined ? count : 0;
        subStringTable.set(highChar, existingCount - 1 | 0);
      }
      _highIndex = highIndex + 1 | 0;
      continue ;
    };
  };
  var subStringTable = makeSubStringTable(subString);
  var $$char = str[0];
  var firstChar = $$char !== undefined ? $$char : "";
  if (subStringTable.has(firstChar)) {
    var count = subStringTable.get(firstChar);
    var existingCount = count !== undefined ? count : 0;
    subStringTable.set(firstChar, existingCount + 1 | 0);
  }
  var match = expandWindow(subStringTable, 0, 1, 1);
  var _startIndices = [];
  var _subStringTable = match[0];
  var _lowIndex = 0;
  var _highIndex = match[1];
  while(true) {
    var highIndex = _highIndex;
    var lowIndex = _lowIndex;
    var subStringTable$1 = _subStringTable;
    var startIndices = _startIndices;
    var counts = Core__Array.reduce(Array.from(subStringTable$1.entries()), 0, (function (acc, param) {
            return acc + param[1] | 0;
          }));
    if (highIndex === stringLength) {
      if (counts === 0) {
        return startIndices.concat([lowIndex]);
      } else {
        return startIndices;
      }
    }
    if (lowIndex === highIndex) {
      _highIndex = highIndex + 1 | 0;
      continue ;
    }
    if (counts === 0) {
      var initialSubStringTable = makeSubStringTable(subString);
      var newLowIndex = highIndex + 1 | 0;
      var $$char$1 = str[newLowIndex];
      var lowChar = $$char$1 !== undefined ? $$char$1 : "";
      if (initialSubStringTable.has(lowChar)) {
        var count$1 = initialSubStringTable.get(lowChar);
        var existingCount$1 = count$1 !== undefined ? count$1 : 0;
        initialSubStringTable.set(lowChar, existingCount$1 - 1 | 0);
      }
      var match$1 = expandWindow(initialSubStringTable, lowIndex, newLowIndex, highIndex + 2 | 0);
      _highIndex = match$1[1];
      _lowIndex = newLowIndex;
      _subStringTable = match$1[0];
      _startIndices = startIndices.concat([lowIndex]);
      continue ;
    }
    var $$char$2 = str[lowIndex];
    var lowChar$1 = $$char$2 !== undefined ? $$char$2 : "";
    if (subStringTable$1.has(lowChar$1)) {
      var count$2 = subStringTable$1.get(lowChar$1);
      var existingCount$2 = count$2 !== undefined ? count$2 : 0;
      subStringTable$1.set(lowChar$1, existingCount$2 + 1 | 0);
    }
    var $$char$3 = str[highIndex];
    var highChar = $$char$3 !== undefined ? $$char$3 : "";
    if (subStringTable$1.has(highChar)) {
      var count$3 = subStringTable$1.get(highChar);
      var existingCount$3 = count$3 !== undefined ? count$3 : 0;
      subStringTable$1.set(highChar, existingCount$3 - 1 | 0);
    }
    _highIndex = highIndex + 1 | 0;
    _lowIndex = lowIndex + 1 | 0;
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
