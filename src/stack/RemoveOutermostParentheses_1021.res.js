// Generated by ReScript, PLEASE EDIT WITH CARE


function removeOutermostParentheses(parens) {
  var parensLength = parens.length;
  var _primitiveParens = "";
  var _charStack = "";
  var _openCount = 0;
  var _closeCount = 0;
  var _index = 0;
  while(true) {
    var index = _index;
    var closeCount = _closeCount;
    var openCount = _openCount;
    var charStack = _charStack;
    var primitiveParens = _primitiveParens;
    var charStackLength = charStack.length;
    if (index === parensLength) {
      return primitiveParens;
    }
    if (openCount === 1 && closeCount === 1) {
      _index = index + 1 | 0;
      _closeCount = 0;
      _openCount = 0;
      continue ;
    }
    if (openCount === closeCount && openCount > 1 && closeCount > 1) {
      _index = index + 1 | 0;
      _closeCount = 0;
      _openCount = 0;
      _charStack = "";
      _primitiveParens = primitiveParens.concat(charStack.slice(1, charStackLength - 1 | 0));
      continue ;
    }
    var $$char = parens[index];
    var currentParens = $$char !== undefined ? $$char : "";
    if (currentParens === "(") {
      _index = index + 1 | 0;
      _openCount = openCount + 1 | 0;
      continue ;
    }
    _index = index + 1 | 0;
    _closeCount = closeCount + 1 | 0;
    continue ;
  };
}

var s1 = "(()())(())";

var r1 = removeOutermostParentheses(s1);

console.log("r1 ==  ()()(): ", r1);

var s2 = "(()())(())(()(()))";

var r2 = removeOutermostParentheses(s2);

console.log("r2 ==  ()()()()(()): ", r2);

var s3 = "()()";

var r3 = removeOutermostParentheses(s3);

console.log("r3 ==  : ", r3);

export {
  removeOutermostParentheses ,
  s1 ,
  r1 ,
  s2 ,
  r2 ,
  s3 ,
  r3 ,
}
/* r1 Not a pure module */
