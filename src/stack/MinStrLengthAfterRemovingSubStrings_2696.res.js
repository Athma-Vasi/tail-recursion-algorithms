// Generated by ReScript, PLEASE EDIT WITH CARE


function minStrLengthAfterRemovingSubStrings(str) {
  var strLength = str.length;
  var _stack = "";
  var _index = 0;
  while(true) {
    var index = _index;
    var stack = _stack;
    var stackLength = stack.length;
    var currentChar = str.charAt(index);
    if (index === strLength) {
      return stackLength;
    }
    var match = str.charAt(index);
    switch (match) {
      case "B" :
          var match$1 = stack.charAt(stackLength - 1 | 0);
          if (match$1 === "A") {
            _index = index + 1 | 0;
            _stack = stack.slice(0, stackLength - 1 | 0);
            continue ;
          }
          _index = index + 1 | 0;
          _stack = stack.concat(currentChar);
          continue ;
      case "D" :
          var match$2 = stack.charAt(stackLength - 1 | 0);
          if (match$2 === "C") {
            _index = index + 1 | 0;
            _stack = stack.slice(0, stackLength - 1 | 0);
            continue ;
          }
          _index = index + 1 | 0;
          _stack = stack.concat(currentChar);
          continue ;
      default:
        _index = index + 1 | 0;
        _stack = stack.concat(currentChar);
        continue ;
    }
  };
}

var s1 = "ABFCACDB";

var r1 = minStrLengthAfterRemovingSubStrings(s1);

console.log("r1: ", r1);

var s2 = "ACBBD";

var r2 = minStrLengthAfterRemovingSubStrings(s2);

console.log("r2: ", r2);

export {
  minStrLengthAfterRemovingSubStrings ,
  s1 ,
  r1 ,
  s2 ,
  r2 ,
}
/* r1 Not a pure module */