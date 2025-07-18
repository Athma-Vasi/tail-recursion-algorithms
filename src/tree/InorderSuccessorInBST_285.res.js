// Generated by ReScript, PLEASE EDIT WITH CARE

import * as TreeNode from "./TreeNode.res.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";

function inorderSuccessorInBST(root, p) {
  var _successor;
  var _curr = root;
  var _state = "NotFound";
  var _stack = /* [] */0;
  while(true) {
    var stack = _stack;
    var state = _state;
    var curr = _curr;
    var successor = _successor;
    if (curr !== undefined) {
      if (p === curr.val) {
        _state = "FoundVal";
        _curr = undefined;
        continue ;
      }
      _stack = {
        hd: curr,
        tl: stack
      };
      _curr = curr.left;
      continue ;
    }
    switch (state) {
      case "FoundVal" :
          if (!stack) {
            return successor;
          }
          _stack = /* [] */0;
          _state = "FoundSuccessor";
          _curr = undefined;
          _successor = stack.hd.val;
          continue ;
      case "NotFound" :
          if (!stack) {
            return successor;
          }
          _stack = stack.tl;
          _curr = stack.hd.right;
          continue ;
      case "FoundSuccessor" :
          return successor;
      
    }
  };
}

var tree1 = TreeNode.make(2, Caml_option.some(TreeNode.make(1, undefined, undefined)), Caml_option.some(TreeNode.make(3, undefined, undefined)));

var r1 = inorderSuccessorInBST(tree1, 1);

console.log("r1:", r1);

var tree2 = TreeNode.make(5, Caml_option.some(TreeNode.make(3, Caml_option.some(TreeNode.make(2, Caml_option.some(TreeNode.make(1, undefined, undefined)), undefined)), Caml_option.some(TreeNode.make(4, undefined, undefined)))), Caml_option.some(TreeNode.make(6, undefined, undefined)));

var r2 = inorderSuccessorInBST(tree2, 6);

console.log("r2:", r2);

export {
  inorderSuccessorInBST ,
  tree1 ,
  r1 ,
  tree2 ,
  r2 ,
}
/* tree1 Not a pure module */
