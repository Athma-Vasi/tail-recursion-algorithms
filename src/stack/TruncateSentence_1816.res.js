// Generated by ReScript, PLEASE EDIT WITH CARE


function truncateSentence(str, k) {
  var _charStack = "";
  var _index = 0;
  var _spacesCount = 0;
  while(true) {
    var spacesCount = _spacesCount;
    var index = _index;
    var charStack = _charStack;
    if (index === str.length || spacesCount === k) {
      return charStack;
    }
    var $$char = str.charAt(index);
    _spacesCount = $$char === " " ? spacesCount + 1 | 0 : spacesCount;
    _index = index + 1 | 0;
    _charStack = charStack.concat($$char);
    continue ;
  };
}

var s1 = "Hello how are you Contestant";

var r1 = truncateSentence(s1, 4);

console.log("r1: ", r1);

var s2 = "What is the solution to this problem";

var r2 = truncateSentence(s2, 4);

console.log("r2: ", r2);

var s3 = "chopper is not a tanuki";

var r3 = truncateSentence(s3, 5);

console.log("r3: ", r3);

var k1 = 4;

var k2 = 4;

var k3 = 5;

export {
  truncateSentence ,
  s1 ,
  k1 ,
  r1 ,
  s2 ,
  k2 ,
  r2 ,
  s3 ,
  k3 ,
  r3 ,
}
/* r1 Not a pure module */
