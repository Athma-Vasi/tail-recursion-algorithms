// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Core__Array from "@rescript/core/src/Core__Array.res.js";

function existenceOfSubstringInStringAndItsReverse(str) {
  var strLength = str.length;
  var makeSubstringSet = function (substrSet, str, length, _index) {
    while(true) {
      var index = _index;
      if (index === ((strLength - length | 0) + 1 | 0)) {
        return substrSet;
      }
      var substr = str.slice(index, index + length | 0);
      substrSet.add(substr);
      _index = index + 1 | 0;
      continue ;
    };
  };
  var reverseString = function (_reversed, str, _index) {
    while(true) {
      var index = _index;
      var reversed = _reversed;
      if (index < 0) {
        return reversed;
      }
      var $$char = str.charAt(index);
      _index = index - 1 | 0;
      _reversed = reversed.concat($$char);
      continue ;
    };
  };
  var substringSet = makeSubstringSet(new Set(), str, 2, 0);
  var reversed = reverseString(String(), str, strLength - 1 | 0);
  var reversedSubstringSet = makeSubstringSet(new Set(), reversed, 2, 0);
  var isIntersection = false;
  return Core__Array.reduce(Array.from(substringSet.values()), isIntersection, (function (result, substring) {
                if (reversedSubstringSet.has(substring)) {
                  return true;
                } else {
                  return result;
                }
              }));
}

var s1 = "leetcode";

var r1 = existenceOfSubstringInStringAndItsReverse(s1);

console.log("r1: ", r1);

var s2 = "abcba";

var r2 = existenceOfSubstringInStringAndItsReverse(s2);

console.log("r2: ", r2);

var s3 = "abcd";

var r3 = existenceOfSubstringInStringAndItsReverse(s3);

console.log("r3: ", r3);

export {
  existenceOfSubstringInStringAndItsReverse ,
  s1 ,
  r1 ,
  s2 ,
  r2 ,
  s3 ,
  r3 ,
}
/* r1 Not a pure module */
