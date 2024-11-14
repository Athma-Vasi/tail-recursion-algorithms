// Generated by ReScript, PLEASE EDIT WITH CARE


function findIfDigitGameCanBeWon(nums) {
  var _singlesCount = 0;
  var _doublesCount = 0;
  var _index = 0;
  while(true) {
    var index = _index;
    var doublesCount = _doublesCount;
    var singlesCount = _singlesCount;
    if (index === nums.length) {
      if (singlesCount > doublesCount) {
        return true;
      } else {
        return doublesCount > singlesCount;
      }
    }
    var n = nums.at(index);
    var num = n !== undefined ? n : -1;
    if (num < 10) {
      _index = index + 1 | 0;
      _singlesCount = singlesCount + num | 0;
      continue ;
    }
    _index = index + 1 | 0;
    _doublesCount = doublesCount + num | 0;
    continue ;
  };
}

var n1 = [
  1,
  2,
  3,
  4,
  10
];

var r1 = findIfDigitGameCanBeWon(n1);

console.log("r1: ", r1);

var n2 = [
  1,
  2,
  3,
  4,
  5,
  14
];

var r2 = findIfDigitGameCanBeWon(n2);

console.log("r2: ", r2);

var n3 = [
  5,
  5,
  5,
  25
];

var r3 = findIfDigitGameCanBeWon(n3);

console.log("r3: ", r3);

export {
  findIfDigitGameCanBeWon ,
  n1 ,
  r1 ,
  n2 ,
  r2 ,
  n3 ,
  r3 ,
}
/* r1 Not a pure module */