// Generated by ReScript, PLEASE EDIT WITH CARE


function findWinningPlayerInCoinGame(x, y) {
  var _player = "Alice";
  var _xRemainder = x;
  var _yRemainder = y;
  while(true) {
    var yRemainder = _yRemainder;
    var xRemainder = _xRemainder;
    var player = _player;
    if (xRemainder < 2 && yRemainder < 4 || yRemainder < 4) {
      if (player === "Alice") {
        return "Bob";
      } else {
        return "Alice";
      }
    }
    _yRemainder = yRemainder - 4 | 0;
    _xRemainder = xRemainder - 1 | 0;
    _player = player === "Alice" ? "Bob" : "Alice";
    continue ;
  };
}

var r1 = findWinningPlayerInCoinGame(2, 7);

console.log("r1: ", r1);

var r2 = findWinningPlayerInCoinGame(4, 11);

console.log("r2: ", r2);

var x1 = 2;

var y1 = 7;

var x2 = 4;

var y2 = 11;

export {
  findWinningPlayerInCoinGame ,
  x1 ,
  y1 ,
  r1 ,
  x2 ,
  y2 ,
  r2 ,
}
/* r1 Not a pure module */
