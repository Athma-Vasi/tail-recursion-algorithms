// Generated by ReScript, PLEASE EDIT WITH CARE


function product(numbers) {
  var _accumulator = 1;
  var _rest = numbers;
  while(true) {
    var rest = _rest;
    var accumulator = _accumulator;
    var length = rest.length;
    if (length === 0) {
      return accumulator;
    }
    var number = rest[0];
    var head = number !== undefined ? number : 1;
    var tail = rest.slice(1, length);
    _rest = tail;
    _accumulator = Math.imul(accumulator, head);
    continue ;
  };
}

var numbers = [
  1,
  2,
  3,
  4,
  5
];

var result = product(numbers);

console.log("product", result);

export {
  product ,
  numbers ,
  result ,
}
/* result Not a pure module */