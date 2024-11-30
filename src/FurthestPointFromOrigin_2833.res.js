// Generated by ReScript, PLEASE EDIT WITH CARE


function furthestPointFromOrigin(moves) {
  var _result = 0;
  var _distance = 0;
  var _index = 0;
  while(true) {
    var index = _index;
    var distance = _distance;
    var result = _result;
    if (index === moves.length) {
      if (result < 0) {
        return Math.imul(result, -1) + distance | 0;
      } else {
        return result + distance | 0;
      }
    }
    var move = moves.charAt(index);
    switch (move) {
      case "L" :
          _index = index + 1 | 0;
          _result = result - 1 | 0;
          continue ;
      case "R" :
          _index = index + 1 | 0;
          _result = result + 1 | 0;
          continue ;
      default:
        _index = index + 1 | 0;
        _distance = distance + 1 | 0;
        continue ;
    }
  };
}

var m1 = "L_RL__R";

var r1 = furthestPointFromOrigin(m1);

console.log("r1: ", r1);

var m2 = "_R__LL_";

var r2 = furthestPointFromOrigin(m2);

console.log("r2: ", r2);

var m3 = "_______";

var r3 = furthestPointFromOrigin(m3);

console.log("r3: ", r3);

export {
  furthestPointFromOrigin ,
  m1 ,
  r1 ,
  m2 ,
  r2 ,
  m3 ,
  r3 ,
}
/* r1 Not a pure module */
