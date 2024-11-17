// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Core__Array from "@rescript/core/src/Core__Array.res.js";

function validWord(word) {
  var match = Core__Array.reduce("bcdfghklmnpqrstvwxyz".split(""), [
        new Set(),
        new Set()
      ], (function (sets, $$char) {
          var upper = sets[1];
          var lower = sets[0];
          lower.add($$char);
          upper.add($$char.toUpperCase());
          return [
                  lower,
                  upper
                ];
        }));
  var upperLettersSet = match[1];
  var lowerLettersSet = match[0];
  var match$1 = Core__Array.reduce("aeiou".split(""), [
        new Set(),
        new Set()
      ], (function (sets, $$char) {
          var upper = sets[1];
          var lower = sets[0];
          lower.add($$char);
          upper.add($$char.toUpperCase());
          return [
                  lower,
                  upper
                ];
        }));
  var upperVowelsSet = match$1[1];
  var lowerVowelsSet = match$1[0];
  var specialsSet = Core__Array.reduce("@#$".split(""), new Set(), (function (set, $$char) {
          set.add($$char);
          return set;
        }));
  var wordLength = word.length;
  var isVowels = new Set();
  var isConsonants = new Set();
  var isInvalids = new Set();
  var _index = 0;
  while(true) {
    var index = _index;
    if (wordLength < 3) {
      return false;
    }
    if (index === wordLength) {
      if (isVowels.has(true) && isConsonants.has(true)) {
        return !isInvalids.has(true);
      } else {
        return false;
      }
    }
    var $$char = word.charAt(index);
    var isVowel = lowerVowelsSet.has($$char) || upperVowelsSet.has($$char);
    var isConsonant = lowerLettersSet.has($$char) || upperLettersSet.has($$char);
    var isInvalid = specialsSet.has($$char);
    isVowels.add(isVowel);
    isConsonants.add(isConsonant);
    isInvalids.add(isInvalid);
    _index = index + 1 | 0;
    continue ;
  };
}

var w1 = "234Adas";

var r1 = validWord(w1);

console.log("r1: ", r1);

var w2 = "b3";

var r2 = validWord(w2);

console.log("r2: ", r2);

var w3 = "a3$e";

var r3 = validWord(w3);

console.log("r3: ", r3);

export {
  validWord ,
  w1 ,
  r1 ,
  w2 ,
  r2 ,
  w3 ,
  r3 ,
}
/* r1 Not a pure module */
