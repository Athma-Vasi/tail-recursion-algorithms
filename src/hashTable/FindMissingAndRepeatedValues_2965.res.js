// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Int32 from "rescript/lib/es6/int32.js";
import * as Core__Array from "@rescript/core/src/Core__Array.res.js";

function findMissingAndRepeatedValues(grid) {
  var yLength = grid.length;
  var arr = grid.at(0);
  var row = arr !== undefined ? arr : [];
  var xLength = row.length;
  var rowLoop = function (freqTable, row, _xIndex) {
    while(true) {
      var xIndex = _xIndex;
      if (xIndex === xLength) {
        return freqTable;
      }
      var n = row.at(xIndex);
      var num = n !== undefined ? n : 0;
      var c = freqTable.get(num);
      var existingCount = c !== undefined ? c : 0;
      freqTable.set(num, existingCount + 1 | 0);
      _xIndex = xIndex + 1 | 0;
      continue ;
    };
  };
  var makeFreqTable = function (_freqTable, _yIndex) {
    while(true) {
      var yIndex = _yIndex;
      var freqTable = _freqTable;
      if (yIndex === yLength) {
        return freqTable;
      }
      var arr = grid.at(yIndex);
      var row = arr !== undefined ? arr : [];
      _yIndex = yIndex + 1 | 0;
      _freqTable = rowLoop(freqTable, row, 0);
      continue ;
    };
  };
  var freqTable = makeFreqTable(new Map(), 0);
  var entries = Array.from(freqTable.entries());
  var match = Core__Array.reduce(entries, [
        Int32.min_int,
        new Set()
      ], (function (acc, entry) {
          var num = entry[0];
          var numSet = acc[1];
          numSet.add(num);
          if (entry[1] > 1) {
            return [
                    num,
                    numSet
                  ];
          } else {
            return [
                    acc[0],
                    numSet
                  ];
          }
        }));
  var findMissing = function (_missing, numSet, _end) {
    while(true) {
      var end = _end;
      var missing = _missing;
      if (end === 0 || missing !== 0) {
        return missing;
      }
      _end = end - 1 | 0;
      _missing = numSet.has(end) ? missing : end;
      continue ;
    };
  };
  var maxNum = Math.imul(xLength, yLength);
  var missing = findMissing(0, match[1], maxNum);
  return [
          match[0],
          missing
        ];
}

var g1 = [
  [
    9,
    1,
    7
  ],
  [
    8,
    9,
    2
  ],
  [
    3,
    4,
    6
  ]
];

var r1 = findMissingAndRepeatedValues(g1);

console.log("r1: ", r1);

var g2 = [
  [
    1,
    3
  ],
  [
    2,
    2
  ]
];

var r2 = findMissingAndRepeatedValues(g2);

console.log("r2: ", r2);

export {
  findMissingAndRepeatedValues ,
  g1 ,
  r1 ,
  g2 ,
  r2 ,
}
/* r1 Not a pure module */