// Generated by ReScript, PLEASE EDIT WITH CARE

import * as TreeNode from "./TreeNode.res.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Core__Array from "@rescript/core/src/Core__Array.res.js";

function constructStringFromBinaryTree(root) {
  if (root !== undefined) {
    var _result = String();
    var _stack = {
      hd: [
        root,
        0
      ],
      tl: /* [] */0
    };
    while(true) {
      var stack = _stack;
      var result = _result;
      if (!stack) {
        return result;
      }
      var match = stack.hd;
      var node = match[0];
      var right = node.right;
      var left = node.left;
      var val = node.val;
      var rest = stack.tl;
      var level = match[1];
      if (left !== undefined) {
        if (right !== undefined) {
          _stack = {
            hd: [
              left,
              level + 1 | 0
            ],
            tl: {
              hd: [
                right,
                level + 1 | 0
              ],
              tl: rest
            }
          };
          _result = level === 0 ? result + val.toString() : result + "(" + val.toString();
          continue ;
        }
        _stack = {
          hd: [
            left,
            level + 1 | 0
          ],
          tl: rest
        };
        _result = level === 0 ? result + val.toString() + "((" : result + "(" + val.toString() + "(";
        continue ;
      }
      if (right !== undefined) {
        _stack = {
          hd: [
            right,
            level + 1 | 0
          ],
          tl: rest
        };
        _result = level === 0 ? result + val.toString() + "(()(" : result + "(" + val.toString() + "()";
        continue ;
      }
      _stack = rest;
      _result = level === 0 ? result + val.toString() : result + "(" + val.toString() + Core__Array.reduce(Core__Array.make(level, ""), String(), (function (acc, _curr) {
                return acc + ")";
              }));
      continue ;
    };
  } else {
    return String();
  }
}

var n5 = TreeNode.make(5, Caml_option.some(TreeNode.make(3, undefined, undefined)), Caml_option.some(TreeNode.make(7, undefined, undefined)));

var n15 = TreeNode.make(15, undefined, Caml_option.some(TreeNode.make(18, undefined, undefined)));

var root1 = TreeNode.make(10, Caml_option.some(n5), Caml_option.some(n15));

var r1 = constructStringFromBinaryTree(root1);

console.log("r1: ", r1);

export {
  constructStringFromBinaryTree ,
  n5 ,
  n15 ,
  root1 ,
  r1 ,
}
/* n5 Not a pure module */
