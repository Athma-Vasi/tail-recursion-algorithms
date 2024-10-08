// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Core__Array from "@rescript/core/src/Core__Array.res.js";

function partitionLabels(str) {
  var length = str.length;
  var charIndexMap = Core__Array.reduceWithIndex(str.split(""), new Map(), (function (mapAcc, $$char, index) {
          mapAcc.set($$char, index);
          return mapAcc;
        }));
  var $$char = str[0];
  var anchorChar = $$char !== undefined ? $$char : "";
  var num = charIndexMap.get(anchorChar);
  var maxAnchorIndex = num !== undefined ? num : -1;
  var _accumulator = [];
  var _anchorIndex = 0;
  var _maxAnchorIndex = maxAnchorIndex;
  var _explorerIndex = 1;
  while(true) {
    var explorerIndex = _explorerIndex;
    var maxAnchorIndex$1 = _maxAnchorIndex;
    var anchorIndex = _anchorIndex;
    var accumulator = _accumulator;
    var $$char$1 = str[explorerIndex];
    var explorerChar = $$char$1 !== undefined ? $$char$1 : "";
    var num$1 = charIndexMap.get(explorerChar);
    var maxExplorerIndex = num$1 !== undefined ? num$1 : -1;
    if (anchorIndex === length) {
      return accumulator;
    }
    if (maxExplorerIndex === maxAnchorIndex$1) {
      var newAnchorIndex = maxExplorerIndex + 1 | 0;
      var newAcc = accumulator.concat([newAnchorIndex - anchorIndex | 0]);
      var $$char$2 = str[newAnchorIndex];
      var newAnchorChar = $$char$2 !== undefined ? $$char$2 : "";
      var num$2 = charIndexMap.get(newAnchorChar);
      var newMaxAnchorIndex = num$2 !== undefined ? num$2 : -1;
      _explorerIndex = newAnchorIndex + 1 | 0;
      _maxAnchorIndex = newMaxAnchorIndex;
      _anchorIndex = newAnchorIndex;
      _accumulator = newAcc;
      continue ;
    }
    if (maxExplorerIndex < maxAnchorIndex$1) {
      _explorerIndex = explorerIndex + 1 | 0;
      continue ;
    }
    _explorerIndex = explorerIndex + 1 | 0;
    _maxAnchorIndex = maxExplorerIndex;
    continue ;
  };
}

var s1 = "ababcbacadefegdehijhklij";

var r1 = partitionLabels(s1);

console.log("ababcbacadefegdehijhklij", r1);

var s2 = "eccbbbbdec";

var r2 = partitionLabels(s2);

console.log("eccbbbbdec", r2);

export {
  partitionLabels ,
  s1 ,
  r1 ,
  s2 ,
  r2 ,
}
/* r1 Not a pure module */
