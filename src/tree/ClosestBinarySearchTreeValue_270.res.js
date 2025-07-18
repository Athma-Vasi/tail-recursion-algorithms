// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml from "rescript/lib/es6/caml.js";
import * as TreeNode from "./TreeNode.res.js";
import * as Core__List from "@rescript/core/src/Core__List.res.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Core__Option from "@rescript/core/src/Core__Option.res.js";

function closestBinarySearchTreeValue(root, target) {
  var inorderTraverse = function (_valuesDistances, _curr, _stack) {
    while(true) {
      var stack = _stack;
      var curr = _curr;
      var valuesDistances = _valuesDistances;
      if (curr !== undefined) {
        _stack = {
          hd: curr,
          tl: stack
        };
        _curr = curr.left;
        continue ;
      }
      if (!stack) {
        return valuesDistances;
      }
      var node = stack.hd;
      var val = node.val;
      var distance = val - target;
      var absDistance = distance < 1.0 ? distance * -1.0 : distance;
      _stack = stack.tl;
      _curr = node.right;
      _valuesDistances = {
        hd: [
          val,
          absDistance
        ],
        tl: valuesDistances
      };
      continue ;
    };
  };
  return Core__Option.mapOr(Core__List.head(Core__List.sort(inorderTraverse(/* [] */0, root, /* [] */0), (function (param, param$1) {
                        return Caml.float_compare(param[1], param$1[1]);
                      }))), -1, (function (param) {
                return param[0];
              }));
}

var tree1 = TreeNode.make(4, Caml_option.some(TreeNode.make(2, Caml_option.some(TreeNode.make(1, undefined, undefined)), Caml_option.some(TreeNode.make(3, undefined, undefined)))), Caml_option.some(TreeNode.make(5, undefined, undefined)));

var r1 = closestBinarySearchTreeValue(tree1, 3.714286);

console.log("r1: ", r1);

export {
  closestBinarySearchTreeValue ,
  tree1 ,
  r1 ,
}
/* tree1 Not a pure module */
