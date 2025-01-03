// Generated by ReScript, PLEASE EDIT WITH CARE


function divisibleAndNonDivisibleSumsDifference(num, divisor) {
  var loop = function (_num1, _num2, _reduced) {
    while(true) {
      var reduced = _reduced;
      var num2 = _num2;
      var num1 = _num1;
      if (reduced === 0.0) {
        return [
                num1,
                num2
              ];
      }
      var isDivisible = reduced % divisor === 0.0;
      if (isDivisible) {
        _reduced = reduced - 1.0;
        _num2 = num2 + reduced;
        continue ;
      }
      _reduced = reduced - 1.0;
      _num1 = num1 + reduced;
      continue ;
    };
  };
  var match = loop(0.0, 0.0, num);
  return match[0] - match[1] | 0;
}

var r1 = divisibleAndNonDivisibleSumsDifference(10, 3);

console.log("r1: ", r1);

var r2 = divisibleAndNonDivisibleSumsDifference(5, 6);

console.log("r2: ", r2);

var r3 = divisibleAndNonDivisibleSumsDifference(5, 1);

console.log("r3: ", r3);

var n1 = 10;

var d1 = 3;

var n2 = 5;

var d2 = 6;

var n3 = 5;

var d3 = 1;

export {
  divisibleAndNonDivisibleSumsDifference ,
  n1 ,
  d1 ,
  r1 ,
  n2 ,
  d2 ,
  r2 ,
  n3 ,
  d3 ,
  r3 ,
}
/* r1 Not a pure module */
