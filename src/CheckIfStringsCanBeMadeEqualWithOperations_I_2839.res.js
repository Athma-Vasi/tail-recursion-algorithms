// Generated by ReScript, PLEASE EDIT WITH CARE


function checkIfStringsCanBeMadeEqualWithOperations_I(s1, s2) {
  var s1ZeroChar = s1.charAt(0);
  var s1FirstChar = s1.charAt(1);
  var s1SecondChar = s1.charAt(2);
  var s1ThirdChar = s1.charAt(3);
  var s2ZeroChar = s2.charAt(0);
  var s2FirstChar = s2.charAt(1);
  var s2SecondChar = s2.charAt(2);
  var s2ThirdChar = s2.charAt(3);
  var evenCheck = s1ZeroChar === s2ZeroChar && s1SecondChar === s2SecondChar || s1ZeroChar === s2SecondChar && s1SecondChar === s2ZeroChar;
  var oddCheck = s1FirstChar === s2FirstChar && s1ThirdChar === s2ThirdChar || s1FirstChar === s2ThirdChar && s1ThirdChar === s2FirstChar;
  if (evenCheck) {
    return oddCheck;
  } else {
    return false;
  }
}

var s1 = "abcd";

var s11 = "cdab";

var r1 = checkIfStringsCanBeMadeEqualWithOperations_I(s1, s11);

console.log("r1: ", r1);

var s2 = "abcd";

var s22 = "dacb";

var r2 = checkIfStringsCanBeMadeEqualWithOperations_I(s2, s22);

console.log("r2: ", r2);

export {
  checkIfStringsCanBeMadeEqualWithOperations_I ,
  s1 ,
  s11 ,
  r1 ,
  s2 ,
  s22 ,
  r2 ,
}
/* r1 Not a pure module */
