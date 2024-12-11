// Generated by ReScript, PLEASE EDIT WITH CARE


function subsets(nums) {
  var numLength = nums.length;
  var _powerSet = [[]];
  var _numIndex = 0;
  while(true) {
    var numIndex = _numIndex;
    var powerSet = _powerSet;
    if (numIndex === numLength) {
      return powerSet;
    }
    var n = nums.at(numIndex);
    var num = n !== undefined ? n : 0;
    var powerLength = powerSet.length;
    console.log("\n");
    console.log("--findPowerSet--");
    console.log("powerSet: ", powerSet);
    console.log("numIndex: ", numIndex);
    console.log("num: ", num);
    console.log("powerLength: ", powerLength);
    var subsetLoop = (function(powerSet,num,powerLength){
    return function subsetLoop(_subset, _subIndex) {
      while(true) {
        var subIndex = _subIndex;
        var subset = _subset;
        if (subIndex === powerLength) {
          return subset;
        }
        var set = powerSet.at(subIndex);
        var currSubset = set !== undefined ? set.concat([num]) : [num];
        console.log("\n");
        console.log("--subsetLoop--");
        console.log("subset: ", subset);
        console.log("subIndex: ", subIndex);
        console.log("currSubset: ", currSubset);
        _subIndex = subIndex + 1 | 0;
        _subset = subset.concat([currSubset]);
        continue ;
      };
    }
    }(powerSet,num,powerLength));
    var subset = subsetLoop([], 0);
    console.log("\n");
    console.log("--findPowerSet--");
    console.log("subset: ", subset);
    _numIndex = numIndex + 1 | 0;
    _powerSet = powerSet.concat(subset);
    continue ;
  };
}

var n1 = [
  1,
  2,
  3
];

var r1 = subsets(n1);

console.log("r1: ", r1);

var n2 = [0];

var r2 = subsets(n2);

console.log("r2: ", r2);

export {
  subsets ,
  n1 ,
  r1 ,
  n2 ,
  r2 ,
}
/* r1 Not a pure module */
