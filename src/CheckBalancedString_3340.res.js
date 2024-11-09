// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Core__Int from "@rescript/core/src/Core__Int.res.js";

function checkBalancedString(numStr) {
  var evenLoop = function (_evenSum, oddSum, _index) {
    while(true) {
      var index = _index;
      var evenSum = _evenSum;
      var isIndexEven = index % 2.0 === 0.0;
      if (index === numStr.length) {
        return [
                evenSum,
                oddSum
              ];
      }
      if (!isIndexEven) {
        var _oddSum = oddSum;
        var _index$1 = index;
        while(true) {
          var index$1 = _index$1;
          var oddSum$1 = _oddSum;
          var isIndexOdd = index$1 % 2.0 !== 0.0;
          if (index$1 === numStr.length) {
            return [
                    evenSum,
                    oddSum$1
                  ];
          }
          if (!isIndexOdd) {
            return evenLoop(evenSum, oddSum$1, index$1);
          }
          var numChar = numStr.charAt(index$1);
          var n = Core__Int.fromString(numChar, undefined);
          var num = n !== undefined ? n : -1;
          _index$1 = index$1 + 1 | 0;
          _oddSum = oddSum$1 + num | 0;
          continue ;
        };
      }
      var numChar$1 = numStr.charAt(index);
      var n$1 = Core__Int.fromString(numChar$1, undefined);
      var num$1 = n$1 !== undefined ? n$1 : -1;
      _index = index + 1 | 0;
      _evenSum = evenSum + num$1 | 0;
      continue ;
    };
  };
  var match = evenLoop(0, 0, 0);
  return match[0] === match[1];
}

var s1 = "1234";

var r1 = checkBalancedString(s1);

console.log("r1: ", r1);

var s2 = "24123";

var r2 = checkBalancedString(s2);

console.log("r2: ", r2);

export {
  checkBalancedString ,
  s1 ,
  r1 ,
  s2 ,
  r2 ,
}
/* r1 Not a pure module */