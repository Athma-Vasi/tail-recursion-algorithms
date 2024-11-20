// Generated by ReScript, PLEASE EDIT WITH CARE


function maxNumberOfOpsWithTheSameScore_I(nums) {
  if (nums.length === 2) {
    return 1;
  } else {
    var _operations = 0;
    var scores = new Set();
    var _sliced = nums;
    while(true) {
      var sliced = _sliced;
      var operations = _operations;
      var length = sliced.length;
      if (scores.size > 1) {
        return operations - 1 | 0;
      }
      if (length < 2) {
        return operations;
      }
      var n = sliced.at(0);
      var first = n !== undefined ? n : 0;
      var n$1 = sliced.at(1);
      var second = n$1 !== undefined ? n$1 : 0;
      scores.add(first + second | 0);
      _sliced = sliced.slice(2, length);
      _operations = operations + 1 | 0;
      continue ;
    };
  }
}

var n1 = [
  3,
  2,
  1,
  4,
  5
];

var r1 = maxNumberOfOpsWithTheSameScore_I(n1);

console.log("r1: ", r1);

var n2 = [
  1,
  5,
  3,
  3,
  4,
  1,
  3,
  2,
  2,
  3
];

var r2 = maxNumberOfOpsWithTheSameScore_I(n2);

console.log("r2: ", r2);

var n3 = [
  5,
  3
];

var r3 = maxNumberOfOpsWithTheSameScore_I(n3);

console.log("r3: ", r3);

export {
  maxNumberOfOpsWithTheSameScore_I ,
  n1 ,
  r1 ,
  n2 ,
  r2 ,
  n3 ,
  r3 ,
}
/* r1 Not a pure module */
