// Generated by ReScript, PLEASE EDIT WITH CARE


function longestStrictlyIncrOrStrictlyDecrSubarray(nums) {
  var makeMonoIncrStack = function (_monoIncrStack, nums, _index) {
    while(true) {
      var index = _index;
      var monoIncrStack = _monoIncrStack;
      if (index === nums.length) {
        return monoIncrStack;
      }
      var n = monoIncrStack.at(-1);
      var prevNum = n !== undefined ? n : 0;
      var n$1 = nums.at(index);
      var currNum = n$1 !== undefined ? n$1 : 0;
      if (currNum > prevNum || prevNum === 0) {
        _index = index + 1 | 0;
        _monoIncrStack = monoIncrStack.concat([currNum]);
        continue ;
      }
      _index = index + 1 | 0;
      continue ;
    };
  };
  var makeMonoDecrStack = function (_monoDecrStack, nums, _index) {
    while(true) {
      var index = _index;
      var monoDecrStack = _monoDecrStack;
      if (index === nums.length) {
        return monoDecrStack;
      }
      var n = monoDecrStack.at(-1);
      var prevNum = n !== undefined ? n : 0;
      var n$1 = nums.at(index);
      var currNum = n$1 !== undefined ? n$1 : 0;
      if (currNum < prevNum || prevNum === 0) {
        _index = index + 1 | 0;
        _monoDecrStack = monoDecrStack.concat([currNum]);
        continue ;
      }
      _index = index + 1 | 0;
      continue ;
    };
  };
  var monoIncrStackLength = makeMonoIncrStack([], nums, 0).length;
  var monoDecrStackLength = makeMonoDecrStack([], nums, 0).length;
  if (monoIncrStackLength > monoDecrStackLength) {
    return monoIncrStackLength;
  } else {
    return monoDecrStackLength;
  }
}

var n1 = [
  1,
  4,
  3,
  3,
  2
];

var r1 = longestStrictlyIncrOrStrictlyDecrSubarray(n1);

console.log("r1: ", r1);

var n2 = [
  3,
  3,
  3,
  3
];

var r2 = longestStrictlyIncrOrStrictlyDecrSubarray(n2);

console.log("r2: ", r2);

var n3 = [
  3,
  2,
  1
];

var r3 = longestStrictlyIncrOrStrictlyDecrSubarray(n3);

console.log("r3: ", r3);

export {
  longestStrictlyIncrOrStrictlyDecrSubarray ,
  n1 ,
  r1 ,
  n2 ,
  r2 ,
  n3 ,
  r3 ,
}
/* r1 Not a pure module */
