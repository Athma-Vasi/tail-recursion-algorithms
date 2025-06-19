// T(n) = O(n)
// S(n) = O(n)

type nodeKind = Leaf | Branch

let smallestSubtreeWithAllTheDeepestNodes = (root: option<TreeNode.t<int>>) => {
  // Postorder traversal collecting nodes in a reverse polish notation style
  // Accumulates: max depth, a stack of (depth, value, nodeKind)
  let rec postorderTraverse = (
    maxDepth: int,
    // Stores each node's depth, value, and kind (Leaf or Branch)
    rpnStack: list<(int, int, nodeKind)>,
    // Stack for traversal: (node, depth)
    workingStack: list<(TreeNode.t<int>, int)>,
  ) => {
    switch workingStack {
    // Traversal finished
    | list{} => (maxDepth, rpnStack)
    // Continue traversal
    | list{(node, depth), ...rest} => {
        let {left, right, val} = node
        let newMaxDepth = maxDepth > depth ? maxDepth : depth

        switch (left, right) {
        // Leaf node: add to stack as Leaf
        | (None, None) =>
          postorderTraverse(newMaxDepth, list{(depth, val, Leaf), ...rpnStack}, rest)
        // Only right child exists: process right next
        | (None, Some(rightNode)) =>
          postorderTraverse(
            newMaxDepth,
            list{(depth, val, Branch), ...rpnStack},
            list{(rightNode, depth + 1), ...rest},
          )
        // Only left child exists: process left next
        | (Some(leftNode), None) =>
          postorderTraverse(
            newMaxDepth,
            list{(depth, val, Branch), ...rpnStack},
            list{(leftNode, depth + 1), ...rest},
          )
        // Both children exist: process left and right next
        | (Some(leftNode), Some(rightNode)) =>
          postorderTraverse(
            newMaxDepth,
            list{(depth, val, Branch), ...rpnStack},
            list{(leftNode, depth + 1), (rightNode, depth + 1), ...rest},
          )
        }
      }
    }
  }

  let rec processRPNStack = (
    evaluationStack: list<int>, // Temporarily holds deepest leaf values
    maxDepth: int, // Max depth found in traversal
    // The stack to evaluate
    rpnStack: list<(int, int, nodeKind)>,
  ) => {
    switch rpnStack {
    | list{} => evaluationStack
    | list{(depth, val, nodeKind), ...rest} =>
      switch (depth === maxDepth, nodeKind) {
      // Deepest leaf node: push onto evaluation stack
      | (true, Leaf) => processRPNStack(list{val, ...evaluationStack}, maxDepth, rest)
      // Not possible path: skip
      | (true, Branch) => processRPNStack(evaluationStack, maxDepth, rest)
      // Non-deepest leaf: skip
      | (false, Leaf) => processRPNStack(evaluationStack, maxDepth, rest)
      // Possible ancestor of deepest leaves
      | (false, Branch) =>
        processRPNStack(
          // If it's one level above max depth, ancestor candidate
          depth === maxDepth - 1 ? list{val, ...evaluationStack} : evaluationStack,
          maxDepth,
          rest,
        )
      }
    }
  }

  switch root {
  | None => [] // Empty tree
  | Some(node) => {
      // Traverse to build RPN and determine max depth
      let (maxDepth, rpnStack) = postorderTraverse(-1, list{}, list{(node, 0)})
      // Process RPN stack to find LCA(s) of deepest leaves
      processRPNStack(list{}, maxDepth, rpnStack)->List.toArray
    }
  }
}

/**
        3
      /   \
     5     1
    / \   / \
   6   2 0   8
      / \
     7   4
 */
let node7 = TreeNode.make(~val=7)
let node4 = TreeNode.make(~val=4)
let node2 = TreeNode.make(~val=2, ~left=Some(node7), ~right=Some(node4))
let node6 = TreeNode.make(~val=6)
let node0 = TreeNode.make(~val=0)
let node8 = TreeNode.make(~val=8)
let node5 = TreeNode.make(~val=5, ~left=Some(node6), ~right=Some(node2))
let node1 = TreeNode.make(~val=1, ~left=Some(node0), ~right=Some(node8))
let root = Some(TreeNode.make(~val=3, ~left=Some(node5), ~right=Some(node1)))
let r1 = smallestSubtreeWithAllTheDeepestNodes(root)
Console.log2("r1: ", r1) // [2, 7, 4]
