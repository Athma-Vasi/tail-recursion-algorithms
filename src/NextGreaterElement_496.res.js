// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Int32 from "rescript/lib/es6/int32.js";

function nextGreaterElement(nums1, nums2) {
  var nums1Length = nums1.length;
  var nums2Length = nums2.length;
  var _accumulator = [];
  var _nums1Index = 0;
  while(true) {
    var nums1Index = _nums1Index;
    var accumulator = _accumulator;
    var num = nums1[nums1Index];
    var currentNum1 = num !== undefined ? num : Int32.min_int;
    var nums2Index = nums2.findIndex((function(currentNum1){
        return function (num2) {
          return num2 === currentNum1;
        }
        }(currentNum1)));
    var nums2Loop = (function(currentNum1){
    return function nums2Loop(greaterNum, _nums2Index) {
      while(true) {
        var nums2Index = _nums2Index;
        if (nums2Index === (nums2Length - 1 | 0)) {
          return greaterNum;
        }
        var num = nums2[nums2Index];
        var currentNum2 = num !== undefined ? num : Int32.min_int;
        if (currentNum2 > currentNum1) {
          return currentNum2;
        }
        _nums2Index = nums2Index + 1 | 0;
        continue ;
      };
    }
    }(currentNum1));
    var greaterNum = nums2Loop(-1, nums2Index);
    if (nums1Index === nums1Length) {
      return accumulator;
    }
    _nums1Index = nums1Index + 1 | 0;
    _accumulator = accumulator.concat([greaterNum]);
    continue ;
  };
}

var nums1 = [
  4,
  1,
  2
];

var nums2 = [
  1,
  3,
  4,
  2
];

var result1 = nextGreaterElement(nums1, nums2);

console.log("result1", result1);

export {
  nextGreaterElement ,
  nums1 ,
  nums2 ,
  result1 ,
}
/* result1 Not a pure module */