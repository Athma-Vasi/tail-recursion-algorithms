// Generated by ReScript, PLEASE EDIT WITH CARE

import * as TreeNode from "./TreeNode.res.js";
import * as Caml_int32 from "rescript/lib/es6/caml_int32.js";
import * as Core__List from "@rescript/core/src/Core__List.res.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";

function countNodesEqualToAverageOfSubtree(root) {
  var postorderTraverse = function (_rpnStack, _workingStack) {
    while(true) {
      var workingStack = _workingStack;
      var rpnStack = _rpnStack;
      if (!workingStack) {
        return rpnStack;
      }
      var top = workingStack.hd;
      var right = top.right;
      var left = top.left;
      var val = top.val;
      var rest = workingStack.tl;
      if (left !== undefined) {
        if (right !== undefined) {
          _workingStack = {
            hd: right,
            tl: {
              hd: left,
              tl: rest
            }
          };
          _rpnStack = {
            hd: [
              val,
              "Branch"
            ],
            tl: rpnStack
          };
          continue ;
        }
        _workingStack = {
          hd: left,
          tl: rest
        };
        _rpnStack = {
          hd: [
            val,
            "Branch"
          ],
          tl: rpnStack
        };
        continue ;
      }
      if (right !== undefined) {
        _workingStack = {
          hd: right,
          tl: rest
        };
        _rpnStack = {
          hd: [
            val,
            "Branch"
          ],
          tl: rpnStack
        };
        continue ;
      }
      _workingStack = rest;
      _rpnStack = {
        hd: [
          val,
          "Leaf"
        ],
        tl: rpnStack
      };
      continue ;
    };
  };
  if (root !== undefined) {
    var _count = 0;
    var _evaluationStack = /* [] */0;
    var _rpnStack = postorderTraverse(/* [] */0, {
          hd: root,
          tl: /* [] */0
        });
    while(true) {
      var rpnStack = _rpnStack;
      var evaluationStack = _evaluationStack;
      var count = _count;
      if (!rpnStack) {
        return count;
      }
      var rest = rpnStack.tl;
      var match = rpnStack.hd;
      var val = match[0];
      if (match[1] === "Leaf") {
        _rpnStack = rest;
        _evaluationStack = {
          hd: [
            val,
            0
          ],
          tl: evaluationStack
        };
        continue ;
      }
      var match$1 = Core__List.reduceWithIndex(evaluationStack, [
            undefined,
            undefined,
            /* [] */0
          ], (function (acc, curr, idx) {
              var restEval = acc[2];
              var leftValue = acc[0];
              if (idx === 0) {
                return [
                        leftValue,
                        curr,
                        restEval
                      ];
              }
              var rightValue = acc[1];
              if (idx === 1) {
                return [
                        curr,
                        rightValue,
                        restEval
                      ];
              } else {
                return [
                        leftValue,
                        rightValue,
                        {
                          hd: curr,
                          tl: restEval
                        }
                      ];
              }
            }));
      var restEval = match$1[2];
      var rightValue = match$1[1];
      var leftValue = match$1[0];
      if (leftValue !== undefined) {
        var subTreeCount = leftValue[1];
        var leftVal = leftValue[0];
        if (rightValue !== undefined) {
          var rightSubTreeCount = rightValue[1];
          var sum = (leftVal + rightValue[0] | 0) + val | 0;
          var subTreeCount$1 = (
            subTreeCount === 0 ? 1 : subTreeCount
          ) + (
            rightSubTreeCount === 0 ? 1 : rightSubTreeCount + 1 | 0
          ) | 0;
          var isAverage = Caml_int32.div(sum, subTreeCount$1) === val;
          _rpnStack = rest;
          _evaluationStack = {
            hd: [
              sum,
              subTreeCount$1 + 1 | 0
            ],
            tl: restEval
          };
          _count = isAverage ? count + subTreeCount$1 | 0 : count;
          continue ;
        }
        var sum$1 = leftVal + val | 0;
        var totalSubTreeCount = subTreeCount + 1 | 0;
        var isAverage$1 = Caml_int32.div(sum$1, totalSubTreeCount) === val;
        _rpnStack = rest;
        _evaluationStack = {
          hd: [
            sum$1,
            totalSubTreeCount + 1 | 0
          ],
          tl: restEval
        };
        _count = isAverage$1 ? count + totalSubTreeCount | 0 : count;
        continue ;
      }
      if (rightValue === undefined) {
        return count;
      }
      var sum$2 = rightValue[0] + val | 0;
      var totalSubTreeCount$1 = rightValue[1] + 1 | 0;
      var isAverage$2 = Caml_int32.div(sum$2, totalSubTreeCount$1) === val;
      _rpnStack = rest;
      _evaluationStack = {
        hd: [
          sum$2,
          totalSubTreeCount$1 + 1 | 0
        ],
        tl: restEval
      };
      _count = isAverage$2 ? count + totalSubTreeCount$1 | 0 : count;
      continue ;
    };
  } else {
    return 0;
  }
}

var n5 = TreeNode.make(5, Caml_option.some(TreeNode.make(3, undefined, undefined)), Caml_option.some(TreeNode.make(7, undefined, undefined)));

var n15 = TreeNode.make(15, Caml_option.some(TreeNode.make(13, undefined, undefined)), Caml_option.some(TreeNode.make(18, undefined, undefined)));

var root1 = TreeNode.make(10, Caml_option.some(n5), Caml_option.some(n15));

var r1 = countNodesEqualToAverageOfSubtree(root1);

console.log("r1: ", r1);

var n4 = TreeNode.make(4, Caml_option.some(TreeNode.make(2, undefined, undefined)), Caml_option.some(TreeNode.make(6, undefined, undefined)));

var n12 = TreeNode.make(12, Caml_option.some(TreeNode.make(10, undefined, undefined)), Caml_option.some(TreeNode.make(14, undefined, undefined)));

var root2 = TreeNode.make(8, Caml_option.some(n4), Caml_option.some(n12));

var r2 = countNodesEqualToAverageOfSubtree(root2);

console.log("r2: ", r2);

export {
  countNodesEqualToAverageOfSubtree ,
  n5 ,
  n15 ,
  root1 ,
  r1 ,
  n4 ,
  n12 ,
  root2 ,
  r2 ,
}
/* n5 Not a pure module */
