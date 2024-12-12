// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Int32 from "rescript/lib/es6/int32.js";

function permutations(nums) {
  var result = [];
  var _current = [];
  var _set = new Set(nums);
  var _setIndex = 0;
  while(true) {
    var setIndex = _setIndex;
    var set = _set;
    var current = _current;
    if (set.size === 0 || setIndex >= set.size) {
      return result.concat([current]);
    }
    var v = Array.from(set.values()).at(setIndex);
    var val = v !== undefined ? v : Int32.min_int;
    var setCopy = new Set(set.values());
    setCopy.delete(val);
    console.log("\n");
    console.log("--loop--");
    console.log("result: ", result);
    console.log("current: ", current);
    console.log("set: ", set);
    console.log("setIndex: ", setIndex);
    console.log("val: ", val);
    console.log("setCopy: ", setCopy);
    _setIndex = setIndex + 1 | 0;
    _set = setCopy;
    _current = current.concat([val]);
    continue ;
  };
}

var n1 = [
  1,
  2,
  3
];

var r1 = permutations(n1);

console.log("r1: ", r1);

var n2 = [
  0,
  1
];

var r2 = permutations(n2);

console.log("r2: ", r2);

var n3 = [1];

var r3 = permutations(n3);

console.log("r3: ", r3);

export {
  permutations ,
  n1 ,
  r1 ,
  n2 ,
  r2 ,
  n3 ,
  r3 ,
}
/* r1 Not a pure module */
