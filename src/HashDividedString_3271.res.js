// Generated by ReScript, PLEASE EDIT WITH CARE


function hashDividedString(str, k) {
  var _hashedResult = "";
  var _rest = str;
  var _outerIndex = 0;
  while(true) {
    var outerIndex = _outerIndex;
    var rest = _rest;
    var hashedResult = _hashedResult;
    if (outerIndex === k || rest.length === 0) {
      return hashedResult;
    }
    var innerLoop = function (_sum, subString, _innerIndex) {
      while(true) {
        var innerIndex = _innerIndex;
        var sum = _sum;
        if (innerIndex === subString.length) {
          return sum;
        }
        var charACode = "a".charCodeAt(0);
        var firstChar = subString.charAt(innerIndex);
        var firstCharPosition = firstChar.charCodeAt(0) - charACode;
        _innerIndex = innerIndex + 1 | 0;
        _sum = sum + firstCharPosition;
        continue ;
      };
    };
    var subString = rest.slice(0, k);
    var remaining = rest.slice(k, rest.length);
    var sum = innerLoop(0.0, subString, 0);
    var hashedCharPosition = sum % 26.0;
    var hashedChar = String.fromCharCode(hashedCharPosition + 97.0 | 0);
    _outerIndex = outerIndex + 1 | 0;
    _rest = remaining;
    _hashedResult = hashedResult.concat(hashedChar);
    continue ;
  };
}

var s1 = "abcd";

var r1 = hashDividedString(s1, 2);

console.log("r1: ", r1);

var s2 = "mxz";

var r2 = hashDividedString(s2, 3);

console.log("r2: ", r2);

var k1 = 2;

var k2 = 3;

export {
  hashDividedString ,
  s1 ,
  k1 ,
  r1 ,
  s2 ,
  k2 ,
  r2 ,
}
/* r1 Not a pure module */
