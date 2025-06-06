// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml from "rescript/lib/es6/caml.js";
import * as Core__Option from "@rescript/core/src/Core__Option.res.js";

function findMostFrequentVowelAndConsonant(s) {
  var makeStringSet = function (str) {
    var set = new Set();
    var _index = 0;
    while(true) {
      var index = _index;
      if (index === str.length) {
        return set;
      }
      var $$char = str.charAt(index);
      set.add($$char);
      _index = index + 1 | 0;
      continue ;
    };
  };
  var vowelsSet = makeStringSet("aeiou");
  var splitVowelsAndConsonants = function (str) {
    var _vowels = String();
    var _consonants = String();
    var _index = 0;
    while(true) {
      var index = _index;
      var consonants = _consonants;
      var vowels = _vowels;
      if (index === str.length) {
        return [
                vowels,
                consonants
              ];
      }
      var $$char = str.charAt(index);
      var isVowel = vowelsSet.has($$char);
      if (isVowel) {
        _index = index + 1 | 0;
        _vowels = vowels.concat($$char);
        continue ;
      }
      _index = index + 1 | 0;
      _consonants = consonants.concat($$char);
      continue ;
    };
  };
  var match = splitVowelsAndConsonants(s);
  var makeFreqTable = function (str) {
    var freqTable = new Map();
    var _index = 0;
    while(true) {
      var index = _index;
      if (index === str.length) {
        return freqTable;
      }
      var $$char = str.charAt(index);
      var freq = Core__Option.mapOr(freqTable.get($$char), 1, (function (freq) {
              return freq + 1 | 0;
            }));
      freqTable.set($$char, freq);
      _index = index + 1 | 0;
      continue ;
    };
  };
  var findMaxFreq = function (freqTable) {
    return Core__Option.mapOr(Array.from(freqTable.values()).toSorted(function (f1, f2) {
                      return Caml.int_compare(f2, f1);
                    }).at(0), 0, (function (f) {
                  return f;
                }));
  };
  var vowelsFreqTable = makeFreqTable(match[0]);
  var consonantsFreqTable = makeFreqTable(match[1]);
  var maxVowelsFreq = findMaxFreq(vowelsFreqTable);
  var maxConsonantsFreq = findMaxFreq(consonantsFreqTable);
  return maxVowelsFreq + maxConsonantsFreq | 0;
}

var s1 = "successes";

var r1 = findMostFrequentVowelAndConsonant(s1);

console.log("r1: ", r1);

var s2 = "aeiaeia";

var r2 = findMostFrequentVowelAndConsonant(s2);

console.log("r2: ", r2);

export {
  findMostFrequentVowelAndConsonant ,
  s1 ,
  r1 ,
  s2 ,
  r2 ,
}
/* r1 Not a pure module */
