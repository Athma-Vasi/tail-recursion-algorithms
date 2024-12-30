// Generated by ReScript, PLEASE EDIT WITH CARE


function sumMultiples(n) {
  var _answer = 0;
  var _counter = 1;
  while(true) {
    var counter = _counter;
    var answer = _answer;
    if (counter > n) {
      return answer;
    }
    var isDivisibleBy3 = counter % 3.0 === 0.0;
    var isDivisibleBy5 = counter % 5.0 === 0.0;
    var isDivisibleBy7 = counter % 7.0 === 0.0;
    _counter = counter + 1 | 0;
    _answer = isDivisibleBy3 || isDivisibleBy5 || isDivisibleBy7 ? answer + counter | 0 : answer;
    continue ;
  };
}

var r1 = sumMultiples(7);

console.log("r1: ", r1);

var r2 = sumMultiples(10);

console.log("r2: ", r2);

var r3 = sumMultiples(9);

console.log("r3: ", r3);

var n1 = 7;

var n2 = 10;

var n3 = 9;

export {
  sumMultiples ,
  n1 ,
  r1 ,
  n2 ,
  r2 ,
  n3 ,
  r3 ,
}
/* r1 Not a pure module */
