// Generated by ReScript, PLEASE EDIT WITH CARE

import * as TreeNode from "./TreeNode.res.js";
import * as Core__List from "@rescript/core/src/Core__List.res.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";

function allElementsInTwoBinarySearchTrees(root1, root2) {
  var inorderTraverse = function (_resultStack, _curr1, _curr2, _stack1, _stack2) {
    while(true) {
      var stack2 = _stack2;
      var stack1 = _stack1;
      var curr2 = _curr2;
      var curr1 = _curr1;
      var resultStack = _resultStack;
      if (curr1 !== undefined) {
        if (curr2 !== undefined) {
          _stack2 = {
            hd: curr2,
            tl: stack2
          };
          _stack1 = {
            hd: curr1,
            tl: stack1
          };
          _curr2 = curr2.left;
          _curr1 = curr1.left;
          continue ;
        }
        _stack1 = {
          hd: curr1,
          tl: stack1
        };
        _curr1 = curr1.left;
        continue ;
      }
      if (curr2 !== undefined) {
        _stack2 = {
          hd: curr2,
          tl: stack2
        };
        _curr2 = curr2.left;
        continue ;
      }
      if (stack1) {
        var rest1 = stack1.tl;
        var top1 = stack1.hd;
        if (stack2) {
          var top2 = stack2.hd;
          _stack2 = stack2.tl;
          _stack1 = rest1;
          _curr2 = top2.right;
          _curr1 = top1.right;
          _resultStack = top1.val < top2.val ? ({
                hd: top1.val,
                tl: {
                  hd: top2.val,
                  tl: resultStack
                }
              }) : ({
                hd: top2.val,
                tl: {
                  hd: top1.val,
                  tl: resultStack
                }
              });
          continue ;
        }
        _stack1 = rest1;
        _curr1 = top1.right;
        _resultStack = {
          hd: top1.val,
          tl: resultStack
        };
        continue ;
      }
      if (!stack2) {
        return resultStack;
      }
      var top2$1 = stack2.hd;
      _stack2 = stack2.tl;
      _curr2 = top2$1.right;
      _resultStack = {
        hd: top2$1.val,
        tl: resultStack
      };
      continue ;
    };
  };
  return Core__List.reverse(inorderTraverse(/* [] */0, root1, root2, /* [] */0, /* [] */0));
}

var n5 = TreeNode.make(5, Caml_option.some(TreeNode.make(3, undefined, undefined)), Caml_option.some(TreeNode.make(7, undefined, undefined)));

var n15 = TreeNode.make(15, undefined, Caml_option.some(TreeNode.make(18, undefined, undefined)));

var root1 = TreeNode.make(10, Caml_option.some(n5), Caml_option.some(n15));

var r1 = allElementsInTwoBinarySearchTrees(root1, n5);

console.log("r1: ", r1);

export {
  allElementsInTwoBinarySearchTrees ,
  n5 ,
  n15 ,
  root1 ,
  r1 ,
}
/* n5 Not a pure module */
