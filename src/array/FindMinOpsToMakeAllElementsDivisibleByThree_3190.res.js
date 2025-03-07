// Generated by ReScript, PLEASE EDIT WITH CARE


function findMinOpsToMakeAllElementsDivisibleByThree(nums) {
  var _operations = 0;
  var _index = 0;
  while(true) {
    var index = _index;
    var operations = _operations;
    if (index === nums.length) {
      return operations;
    }
    var n = nums.at(index);
    var num = n !== undefined ? n : 0;
    if (num % 3.0 === 0.0) {
      _index = index + 1 | 0;
      continue ;
    }
    _index = index + 1 | 0;
    _operations = operations + 1 | 0;
    continue ;
  };
}

var n1 = [
  1,
  2,
  3,
  4
];

var r1 = findMinOpsToMakeAllElementsDivisibleByThree(n1);

console.log("r1: ", r1);

var n2 = [
  3,
  6,
  9
];

var r2 = findMinOpsToMakeAllElementsDivisibleByThree(n2);

console.log("r2: ", r2);

export {
  findMinOpsToMakeAllElementsDivisibleByThree ,
  n1 ,
  r1 ,
  n2 ,
  r2 ,
}
/* r1 Not a pure module */
