// Generated by ReScript, PLEASE EDIT WITH CARE


function numberOfStringsAsSubstrings(patterns, word) {
  var length = patterns.length;
  var _count = 0;
  var _index = 0;
  while(true) {
    var index = _index;
    var count = _count;
    if (index === length) {
      return count;
    }
    var str = patterns.at(index);
    var pattern = str !== undefined ? str : "";
    if (word.includes(pattern)) {
      _index = index + 1 | 0;
      _count = count + 1 | 0;
      continue ;
    }
    _index = index + 1 | 0;
    continue ;
  };
}

var p1 = [
  "a",
  "abc",
  "bc",
  "d"
];

var w1 = "abc";

var r1 = numberOfStringsAsSubstrings(p1, w1);

console.log("[a,abc,bc,d] abc", r1);

var p2 = [
  "a",
  "b",
  "c"
];

var w2 = "aaaaabbbbb";

var r2 = numberOfStringsAsSubstrings(p2, w2);

console.log("[a,b,c] aaaaabbbbb", r2);

var p3 = [
  "a",
  "a",
  "a"
];

var w3 = "ab";

var r3 = numberOfStringsAsSubstrings(p3, w3);

console.log("[a,a,a] ab", r3);

export {
  numberOfStringsAsSubstrings ,
  p1 ,
  w1 ,
  r1 ,
  p2 ,
  w2 ,
  r2 ,
  p3 ,
  w3 ,
  r3 ,
}
/* r1 Not a pure module */
