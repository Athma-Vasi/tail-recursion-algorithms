// T(n) = O(n)
// S(n) = O(n)

type nodeKind = Leaf | Branch

let invertBinaryTree = (root: option<TreeNode.t<int>>) => {
  // Postorder traversal that builds:
  // - rpnStack: list of nodes with their kinds in Reverse Polish Notation (postorder)
  let rec postorderTraverse = (
    rpnStack: list<(int, nodeKind, int)>, // (val, nodeKind, leavesCount )
    workingStack: list<TreeNode.t<int>>,
  ) => {
    switch workingStack {
    // If no more nodes to process, return the stack and table
    | list{} => rpnStack
    | list{top, ...rest} => {
        let {left, right, val} = top

        switch (left, right) {
        // Case 1: No children → it's a leaf
        | (None, None) => postorderTraverse(list{(val, Leaf, 0), ...rpnStack}, rest)

        // Case 2: Only right child → one leaf child
        | (None, Some(rightNode)) =>
          postorderTraverse(list{(val, Branch, 1), ...rpnStack}, list{rightNode, ...rest})

        // Case 3: Only left child → one leaf child
        | (Some(leftNode), None) =>
          postorderTraverse(list{(val, Branch, 1), ...rpnStack}, list{leftNode, ...rest})

        // Case 4: Both children → two leaf children
        | (Some(leftNode), Some(rightNode)) =>
          postorderTraverse(
            list{(val, Branch, 2), ...rpnStack},
            // right first to preserve postorder
            list{rightNode, leftNode, ...rest},
          )
        }
      }
    }
  }

  let rec invert = (
    rpnStack: list<(int, nodeKind, int)>,
    inverted: TreeNode.t<int>,
    evalStack: list<(TreeNode.t<int>, nodeKind)>,
  ) => {
    switch rpnStack {
    | list{} => inverted
    | list{(val, nodeKind, leavesCount), ...rest} =>
      switch nodeKind {
      // If it's a leaf, create a new node and push to evalStack
      | Leaf => invert(rest, inverted, list{(TreeNode.make(~val), Leaf), ...evalStack})

      | Branch => {
          // Extract `leavesCount` number of leaf nodes from evalStack
          let (topLeaves, sliced) = evalStack->List.reduceWithIndex((list{}, list{}), (
            acc,
            curr,
            idx,
          ) => {
            let (topLeaves, sliced) = acc
            let (node, nodeKind) = curr

            switch idx < leavesCount && nodeKind === Leaf {
            | true => (list{(node, nodeKind), ...topLeaves}, sliced)
            | false => (topLeaves, list{(node, nodeKind), ...sliced})
            }
          })

          // Construct subtree with the leaves in reversed positions (right becomes left, left becomes right)
          let newSubtree = switch topLeaves {
          // No children
          | list{} => TreeNode.make(~val)

          // One child → set as right
          | list{(leftSubtree, _nodeKind), ...restSubtrees} =>
            switch restSubtrees {
            | list{} => TreeNode.make(~val, ~right=Some(leftSubtree))

            // Two children → left becomes right, right becomes left (inversion)
            | list{(rightSubtree, _nodeKind), ..._others} =>
              TreeNode.make(~val, ~right=Some(leftSubtree), ~left=Some(rightSubtree))
            }
          }

          // If this was the final node, return as inverted root
          // Otherwise, push it back into the eval stack for higher nodes
          switch List.size(rest) === 0 {
          | true => invert(rest, newSubtree, sliced)
          | false => invert(rest, inverted, list{(newSubtree, Leaf), ...sliced})
          }
        }
      }
    }
  }

  // Default node returned for None input
  let default = TreeNode.make(~val=Int32.min_int)

  // Start by traversing the tree in postorder, then build the inverted tree
  switch root {
  | None => default
  | Some(node) => postorderTraverse(list{}, list{node})->invert(default, list{})
  }
}

/**
       4
     /   \
    2     7
   / \   / \
  1   3 6   9
 */
let tree1 = Some(
  TreeNode.make(
    ~val=4,
    ~left=Some(
      TreeNode.make(~val=2, ~left=Some(TreeNode.make(~val=1)), ~right=Some(TreeNode.make(~val=3))),
    ),
    ~right=Some(
      TreeNode.make(~val=7, ~left=Some(TreeNode.make(~val=6)), ~right=Some(TreeNode.make(~val=9))),
    ),
  ),
)
let r1 = invertBinaryTree(tree1)
Console.log2("r1: ", r1)

/**
   2
  / \
 1   3
 */
let tree2 = Some(
  TreeNode.make(~val=2, ~left=Some(TreeNode.make(~val=1)), ~right=Some(TreeNode.make(~val=3))),
)
let r2 = invertBinaryTree(tree2)
Console.log2("r2: ", r2)
