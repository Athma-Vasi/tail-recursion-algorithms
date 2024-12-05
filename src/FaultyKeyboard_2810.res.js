// Generated by ReScript, PLEASE EDIT WITH CARE


function faultyKeyboard(s) {
  var length = s.length;
  var reverseString = function (original) {
    var _reversed = String();
    var _index = length - 1 | 0;
    while(true) {
      var index = _index;
      var reversed = _reversed;
      if (index < 0) {
        return reversed;
      }
      _index = index - 1 | 0;
      _reversed = reversed.concat(original.charAt(index));
      continue ;
    };
  };
  var _finalString = String();
  var _index = 0;
  while(true) {
    var index = _index;
    var finalString = _finalString;
    if (index === length) {
      return finalString;
    }
    var $$char = s.charAt(index);
    if ($$char === "i") {
      _index = index + 1 | 0;
      _finalString = reverseString(finalString);
      continue ;
    }
    _index = index + 1 | 0;
    _finalString = finalString.concat($$char);
    continue ;
  };
}

var s1 = "string";

var r1 = faultyKeyboard(s1);

console.log("r1: ", r1);

var s2 = "poiinter";

var r2 = faultyKeyboard(s2);

console.log("r2: ", r2);

export {
  faultyKeyboard ,
  s1 ,
  r1 ,
  s2 ,
  r2 ,
}
/* r1 Not a pure module */