// Generated by ReScript, PLEASE EDIT WITH CARE


function removeTrailingZeroesFromAString(numStr) {
  var _stop = false;
  var _index = numStr.length - 1 | 0;
  while(true) {
    var index = _index;
    var stop = _stop;
    if (stop) {
      return numStr.slice(0, index + 1 | 0);
    }
    if (numStr.charAt(index) === "0") {
      _index = index - 1 | 0;
      continue ;
    }
    _stop = true;
    continue ;
  };
}

var n1 = "51230100";

var r1 = removeTrailingZeroesFromAString(n1);

console.log("r1: ", r1);

var n2 = "123";

var r2 = removeTrailingZeroesFromAString(n2);

console.log("r2: ", r2);

export {
  removeTrailingZeroesFromAString ,
  n1 ,
  r1 ,
  n2 ,
  r2 ,
}
/* r1 Not a pure module */
