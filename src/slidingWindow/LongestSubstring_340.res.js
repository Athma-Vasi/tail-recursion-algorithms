// Generated by ReScript, PLEASE EDIT WITH CARE


function longestSubstring340(str, k) {
  var length = str.length;
  if (length < 2) {
    return length;
  }
  var charCountMap = new Map();
  var $$char = str[0];
  var firstChar = $$char !== undefined ? $$char : "";
  var $$char$1 = str[1];
  var secondChar = $$char$1 !== undefined ? $$char$1 : "";
  if (firstChar === secondChar) {
    charCountMap.set(firstChar, 2);
  } else {
    charCountMap.set(firstChar, 1);
    charCountMap.set(secondChar, 1);
  }
  var _longestSubStr = 2;
  var _lowIndex = 0;
  var _highIndex = 1;
  while(true) {
    var highIndex = _highIndex;
    var lowIndex = _lowIndex;
    var longestSubStr = _longestSubStr;
    if (highIndex === (length - 1 | 0)) {
      return longestSubStr;
    }
    if (charCountMap.size > k) {
      var $$char$2 = str[lowIndex];
      var currentLowChar = $$char$2 !== undefined ? $$char$2 : "";
      var num = charCountMap.get(currentLowChar);
      var prevLowCount = num !== undefined ? num : 0;
      var newLowCount = prevLowCount - 1 | 0;
      if (newLowCount === 0) {
        charCountMap.delete(currentLowChar);
      } else {
        charCountMap.set(currentLowChar, newLowCount);
      }
      _lowIndex = lowIndex + 1 | 0;
      _longestSubStr = longestSubStr - 1 | 0;
      continue ;
    }
    var newHighIndex = highIndex + 1 | 0;
    var $$char$3 = str[newHighIndex];
    var newHighChar = $$char$3 !== undefined ? $$char$3 : "";
    var num$1 = charCountMap.get(newHighChar);
    var existingHighCount = num$1 !== undefined ? num$1 : 0;
    charCountMap.set(newHighChar, existingHighCount + 1 | 0);
    _highIndex = newHighIndex;
    _longestSubStr = longestSubStr + 1 | 0;
    continue ;
  };
}

var s1 = "eceba";

var r1 = longestSubstring340(s1, 2);

console.log("eceba", r1);

var s2 = "aa";

var r2 = longestSubstring340(s2, 1);

console.log("aa", r2);

export {
  longestSubstring340 ,
  s1 ,
  r1 ,
  s2 ,
  r2 ,
}
/* r1 Not a pure module */
