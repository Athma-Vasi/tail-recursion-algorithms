// Generated by ReScript, PLEASE EDIT WITH CARE


function timeNeededToBuyTickets(ticketsToBuy, position) {
  var _tickets = ticketsToBuy;
  var _elapsedTime = 0;
  var _index = 0;
  while(true) {
    var index = _index;
    var elapsedTime = _elapsedTime;
    var tickets = _tickets;
    var n = tickets.at(index);
    var ticketWanting = n !== undefined ? n : -1;
    if (index === position && ticketWanting === 1) {
      return elapsedTime + 1 | 0;
    }
    if (index === ticketsToBuy.length) {
      if (ticketWanting >= 0) {
        return elapsedTime;
      }
      _index = 0;
      continue ;
    }
    if (ticketWanting === 0) {
      _index = index + 1 | 0;
      continue ;
    }
    _index = index + 1 | 0;
    _elapsedTime = elapsedTime + 1 | 0;
    _tickets = tickets.map((function(index){
        return function (t, idx) {
          if (idx === index) {
            return t - 1 | 0;
          } else {
            return t;
          }
        }
        }(index)));
    continue ;
  };
}

var t1 = [
  2,
  3,
  2
];

var r1 = timeNeededToBuyTickets(t1, 2);

console.log("r1: ", r1);

var t2 = [
  5,
  1,
  1,
  1
];

var r2 = timeNeededToBuyTickets(t2, 0);

console.log("r2: ", r2);

var p1 = 2;

var p2 = 0;

export {
  timeNeededToBuyTickets ,
  t1 ,
  p1 ,
  r1 ,
  t2 ,
  p2 ,
  r2 ,
}
/* r1 Not a pure module */
