// Generated by ReScript, PLEASE EDIT WITH CARE


function reverseString(str) {
  var _accumulator = "";
  var _tail = str;
  while(true) {
    var tail = _tail;
    var accumulator = _accumulator;
    var length = tail.length;
    if (length === 0) {
      return accumulator;
    }
    var character = tail[0];
    var head = character !== undefined ? character : "";
    var rest = tail.slice(1, length);
    _tail = rest;
    _accumulator = head + accumulator;
    continue ;
  };
}

var string = "Hello, World!";

var result = reverseString(string);

console.log("reversed string", result);

export {
  reverseString ,
  string ,
  result ,
}
/* result Not a pure module */