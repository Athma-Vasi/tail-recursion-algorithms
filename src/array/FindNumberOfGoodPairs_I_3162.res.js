// Generated by ReScript, PLEASE EDIT WITH CARE


function findNumberOfGoodPairs_I(nums1, nums2, k) {
  var _count = 0;
  var _index1 = 0;
  while(true) {
    var index1 = _index1;
    var count = _count;
    if (index1 === nums1.length) {
      return count;
    }
    var n = nums1.at(index1);
    var num1 = n !== undefined ? n : -1;
    var nums2Loop = (function(num1){
    return function nums2Loop(_tempCount, _index2) {
      while(true) {
        var index2 = _index2;
        var tempCount = _tempCount;
        if (index2 === nums2.length) {
          return tempCount;
        }
        var n = nums2.at(index2);
        var num2 = n !== undefined ? n : -1;
        var divisor = Math.imul(num2, k);
        var remainder = num1 % divisor;
        if (remainder === 0.0) {
          _index2 = index2 + 1 | 0;
          _tempCount = tempCount + 1 | 0;
          continue ;
        }
        _index2 = index2 + 1 | 0;
        continue ;
      };
    }
    }(num1));
    _index1 = index1 + 1 | 0;
    _count = nums2Loop(count, 0);
    continue ;
  };
}

var n1 = [
  1,
  3,
  4
];

var n11 = [
  1,
  3,
  4
];

var r1 = findNumberOfGoodPairs_I(n1, n11, 1);

console.log("r1: ", r1);

var n2 = [
  1,
  2,
  4,
  12
];

var n22 = [
  2,
  4
];

var r2 = findNumberOfGoodPairs_I(n2, n22, 3);

console.log("r2: ", r2);

var k1 = 1;

var k2 = 3;

export {
  findNumberOfGoodPairs_I ,
  n1 ,
  n11 ,
  k1 ,
  r1 ,
  n2 ,
  n22 ,
  k2 ,
  r2 ,
}
/* r1 Not a pure module */