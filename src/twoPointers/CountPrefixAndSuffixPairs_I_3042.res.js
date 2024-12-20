// Generated by ReScript, PLEASE EDIT WITH CARE


function countPrefixAndSuffixPairs_I(words) {
  var length = words.length;
  var _count = 0;
  var _leftIndex = 0;
  var _rightIndex = length - 1 | 0;
  while(true) {
    var rightIndex = _rightIndex;
    var leftIndex = _leftIndex;
    var count = _count;
    if (leftIndex > rightIndex || rightIndex === length) {
      return count;
    }
    if (leftIndex === rightIndex) {
      _rightIndex = length - 1 | 0;
      _leftIndex = leftIndex + 1 | 0;
      continue ;
    }
    var w = words.at(leftIndex);
    var leftWord = w !== undefined ? w : String();
    var w$1 = words.at(rightIndex);
    var rightWord = w$1 !== undefined ? w$1 : String();
    var checkAffixesPairs = function (leftWord, rightWord) {
      var checkPrefixes = function (arePrefixesEqual, leftWord, rightWord, _left) {
        while(true) {
          var left = _left;
          if (left === leftWord.length) {
            return !arePrefixesEqual.has(false);
          }
          var leftChar = leftWord.charAt(left);
          var rightChar = rightWord.charAt(left);
          arePrefixesEqual.add(leftChar === rightChar);
          _left = left + 1 | 0;
          continue ;
        };
      };
      var checkSuffixes = function (areSuffixesEqual, leftWord, rightWord, _right) {
        while(true) {
          var right = _right;
          if (right < 0) {
            return !areSuffixesEqual.has(false);
          }
          var leftChar = leftWord.charAt(right);
          var rightChar = rightWord.charAt(right);
          areSuffixesEqual.add(leftChar === rightChar);
          _right = right - 1 | 0;
          continue ;
        };
      };
      var arePrefixesEqual = checkPrefixes(new Set(), leftWord, rightWord, 0);
      var areSuffixesEqual = checkSuffixes(new Set(), leftWord, rightWord, leftWord.length - 1 | 0);
      if (arePrefixesEqual) {
        return areSuffixesEqual;
      } else {
        return false;
      }
    };
    var areAffixesEqual = checkAffixesPairs(leftWord, rightWord);
    _rightIndex = rightIndex - 1 | 0;
    _count = areAffixesEqual ? count + 1 | 0 : count;
    continue ;
  };
}

var w1 = [
  "a",
  "aba",
  "ababa",
  "aa"
];

var r1 = countPrefixAndSuffixPairs_I(w1);

console.log("r1: ", r1);

var w2 = [
  "pa",
  "papa",
  "ma",
  "mama"
];

var r2 = countPrefixAndSuffixPairs_I(w2);

console.log("r2: ", r2);

var w3 = [
  "abab",
  "ab"
];

var r3 = countPrefixAndSuffixPairs_I(w3);

console.log("r3: ", r3);

export {
  countPrefixAndSuffixPairs_I ,
  w1 ,
  r1 ,
  w2 ,
  r2 ,
  w3 ,
  r3 ,
}
/* r1 Not a pure module */
