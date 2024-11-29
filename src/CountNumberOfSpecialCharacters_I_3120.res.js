// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Core__Array from "@rescript/core/src/Core__Array.res.js";

function countNumberOfSpecialCharacters_I(word) {
  var alphabet = Core__Array.reduce("abcdefghijklmnopqrstuvwxyz".split(""), new Set(), (function (set, $$char) {
          set.add($$char);
          return set;
        }));
  var _count = 0;
  var lowercaseSet = new Set();
  var _index = 0;
  while(true) {
    var index = _index;
    var count = _count;
    if (index === word.length) {
      return count;
    }
    var $$char = word.charAt(index);
    if (alphabet.has($$char)) {
      lowercaseSet.add($$char);
      _index = index + 1 | 0;
      continue ;
    }
    _index = index + 1 | 0;
    _count = lowercaseSet.has($$char.toLowerCase()) ? count + 1 | 0 : count;
    continue ;
  };
}

var w1 = "aaAbcBC";

var r1 = countNumberOfSpecialCharacters_I(w1);

console.log("r1: ", r1);

var w2 = "abc";

var r2 = countNumberOfSpecialCharacters_I(w2);

console.log("r2: ", r2);

var w3 = "abBCab";

var r3 = countNumberOfSpecialCharacters_I(w3);

console.log("r3: ", r3);

export {
  countNumberOfSpecialCharacters_I ,
  w1 ,
  r1 ,
  w2 ,
  r2 ,
  w3 ,
  r3 ,
}
/* r1 Not a pure module */