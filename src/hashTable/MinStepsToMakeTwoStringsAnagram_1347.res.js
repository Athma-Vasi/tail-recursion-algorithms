// Generated by ReScript, PLEASE EDIT WITH CARE


function minStepsToMakeTwoStringsAnagram(s, t) {
  var makeFreqTable = function (freqTable, str, _index) {
    while(true) {
      var index = _index;
      if (index === str.length) {
        return freqTable;
      }
      var $$char = str.charAt(index);
      var f = freqTable.get($$char);
      var freq = f !== undefined ? f + 1 | 0 : 1;
      freqTable.set($$char, freq);
      _index = index + 1 | 0;
      continue ;
    };
  };
  var freqTable = makeFreqTable(new Map(), s, 0);
  var _minSteps = 0;
  var _index = 0;
  while(true) {
    var index = _index;
    var minSteps = _minSteps;
    if (index === t.length) {
      return minSteps;
    }
    var $$char = t.charAt(index);
    if (freqTable.has($$char)) {
      var f = freqTable.get($$char);
      var freq = f !== undefined ? f - 1 | 0 : 0;
      freqTable.set($$char, freq);
      _index = index + 1 | 0;
      _minSteps = freq < 0 ? minSteps + 1 | 0 : minSteps;
      continue ;
    }
    _index = index + 1 | 0;
    _minSteps = minSteps + 1 | 0;
    continue ;
  };
}

var s1 = "bab";

var t1 = "aba";

var r1 = minStepsToMakeTwoStringsAnagram(s1, t1);

console.log("r1: ", r1);

var s2 = "leetcode";

var t2 = "practice";

var r2 = minStepsToMakeTwoStringsAnagram(s2, t2);

console.log("r2: ", r2);

var s3 = "anagram";

var t3 = "mangaar";

var r3 = minStepsToMakeTwoStringsAnagram(s3, t3);

console.log("r3: ", r3);

export {
  minStepsToMakeTwoStringsAnagram ,
  s1 ,
  t1 ,
  r1 ,
  s2 ,
  t2 ,
  r2 ,
  s3 ,
  t3 ,
  r3 ,
}
/* r1 Not a pure module */
