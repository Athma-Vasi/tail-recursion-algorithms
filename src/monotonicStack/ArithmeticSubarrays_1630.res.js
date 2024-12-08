// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml from "rescript/lib/es6/caml.js";

function arithmeticSubarrays(numbers, starts, ends) {
  var checkIfArithmeticSequence = function (nums) {
    var length = nums.length;
    var n = nums.at(0);
    var first = n !== undefined ? n : 0;
    var diffs = new Set();
    var _prev = first;
    var _index = 1;
    while(true) {
      var index = _index;
      var prev = _prev;
      if (index === length) {
        return diffs.size === 1;
      }
      var n$1 = nums.at(index);
      var num = n$1 !== undefined ? n$1 : 0;
      diffs.add(prev - num | 0);
      _index = index + 1 | 0;
      _prev = num;
      continue ;
    };
  };
  var _answer = [];
  var _index = 0;
  while(true) {
    var index = _index;
    var answer = _answer;
    if (index === starts.length || index === ends.length) {
      return answer;
    }
    var n = starts.at(index);
    var start = n !== undefined ? n : -1;
    var n$1 = ends.at(index);
    var end = n$1 !== undefined ? n$1 + 1 | 0 : -1;
    var sortedAsc = numbers.slice(start, end).toSorted(Caml.int_compare);
    var isArithmeticSequence = checkIfArithmeticSequence(sortedAsc);
    _index = index + 1 | 0;
    _answer = answer.concat([isArithmeticSequence]);
    continue ;
  };
}

var n1 = [
  4,
  6,
  5,
  9,
  3,
  7
];

var s1 = [
  0,
  0,
  2
];

var e1 = [
  2,
  3,
  5
];

var r1 = arithmeticSubarrays(n1, s1, e1);

console.log("r1: ", r1);

var n2 = [
  -12,
  -9,
  -3,
  -12,
  -6,
  15,
  20,
  -25,
  -20,
  -15,
  -10
];

var s2 = [
  0,
  1,
  6,
  4,
  8,
  7
];

var e2 = [
  4,
  4,
  9,
  7,
  9,
  10
];

var r2 = arithmeticSubarrays(n2, s2, e2);

console.log("r2: ", r2);

export {
  arithmeticSubarrays ,
  n1 ,
  s1 ,
  e1 ,
  r1 ,
  n2 ,
  s2 ,
  e2 ,
  r2 ,
}
/* r1 Not a pure module */