// Generated by ReScript, PLEASE EDIT WITH CARE


function lemonadeChange(bills) {
  var register = new Map();
  var _index = 0;
  while(true) {
    var index = _index;
    if (index === bills.length) {
      return true;
    }
    var b = bills.at(index);
    var bill = b !== undefined ? b : 0;
    var c = register.get(5);
    var fivesCount = c !== undefined ? c : 0;
    var c$1 = register.get(10);
    var tensCount = c$1 !== undefined ? c$1 : 0;
    var c$2 = register.get(20);
    var twentiesCount = c$2 !== undefined ? c$2 : 0;
    if (bill === 0) {
      return false;
    }
    if (bill === 5) {
      register.set(bill, fivesCount + 1 | 0);
      _index = index + 1 | 0;
      continue ;
    }
    var change = bill - 5 | 0;
    if (change === 5) {
      if (fivesCount === 0) {
        return false;
      }
      register.set(5, fivesCount - 1 | 0);
      register.set(10, tensCount + 1 | 0);
      _index = index + 1 | 0;
      continue ;
    }
    if (change !== 15) {
      return false;
    }
    if (tensCount === 0 || fivesCount === 0) {
      return false;
    }
    register.set(5, fivesCount - 1 | 0);
    register.set(10, tensCount - 1 | 0);
    register.set(20, twentiesCount + 1 | 0);
    _index = index + 1 | 0;
    continue ;
  };
}

var b1 = [
  5,
  5,
  5,
  10,
  20
];

var r1 = lemonadeChange(b1);

console.log("r1: ", r1);

var b2 = [
  5,
  5,
  10,
  10,
  20
];

var r2 = lemonadeChange(b2);

console.log("r2: ", r2);

export {
  lemonadeChange ,
  b1 ,
  r1 ,
  b2 ,
  r2 ,
}
/* r1 Not a pure module */
