// Generated by ReScript, PLEASE EDIT WITH CARE

import * as TreeNode from "./TreeNode.res.js";
import * as Core__List from "@rescript/core/src/Core__List.res.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";

function binaryTreePaths(root) {
  var traverse = function (_paths, _stack) {
    while(true) {
      var stack = _stack;
      var paths = _paths;
      if (!stack) {
        return paths;
      }
      var match = stack.hd;
      var poppedNode = match[0];
      var right = poppedNode.right;
      var left = poppedNode.left;
      var rest = stack.tl;
      var path = match[1];
      if (left !== undefined) {
        if (right !== undefined) {
          var leftPath = path.concat("->" + left.val.toString());
          var rightPath = path.concat("->" + right.val.toString());
          _stack = {
            hd: [
              left,
              leftPath
            ],
            tl: {
              hd: [
                right,
                rightPath
              ],
              tl: rest
            }
          };
          continue ;
        }
        var leftPath$1 = path.concat("->" + left.val.toString());
        _stack = {
          hd: [
            left,
            leftPath$1
          ],
          tl: rest
        };
        continue ;
      }
      if (right !== undefined) {
        var rightPath$1 = path.concat("->" + right.val.toString());
        _stack = {
          hd: [
            right,
            rightPath$1
          ],
          tl: rest
        };
        continue ;
      }
      _stack = rest;
      _paths = {
        hd: path,
        tl: paths
      };
      continue ;
    };
  };
  if (root !== undefined) {
    return Core__List.toArray(traverse(/* [] */0, {
                    hd: [
                      root,
                      root.val.toString()
                    ],
                    tl: /* [] */0
                  }));
  } else {
    return [];
  }
}

var n5 = TreeNode.make(5, Caml_option.some(TreeNode.make(3, undefined, undefined)), Caml_option.some(TreeNode.make(7, undefined, undefined)));

var n15 = TreeNode.make(15, undefined, Caml_option.some(TreeNode.make(18, undefined, undefined)));

var root1 = TreeNode.make(10, Caml_option.some(n5), Caml_option.some(n15));

var r1 = binaryTreePaths(root1);

console.log("r1: ", r1);

export {
  binaryTreePaths ,
  n5 ,
  n15 ,
  root1 ,
  r1 ,
}
/* n5 Not a pure module */
