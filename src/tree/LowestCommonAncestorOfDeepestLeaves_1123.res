// T(n) = O(n)
// S(n) = O(n)

type nodeKind = Leaf | Branch

let lowestCommonAncestorOfDeepestLeaves = (root: option<TreeNode.t<int>>) => {
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

  // Processes the RPN stack to find nodes that are common ancestors of deepest leaves
  let rec processRPNStack = (
    ancestorsStack: list<int>, // Final result list of LCA candidates
    evaluationStack: list<int>, // Temporarily holds deepest leaf values
    maxDepth: int, // Max depth found in traversal
    // The stack to evaluate
    rpnStack: list<(int, int, nodeKind)>,
  ) => {
    switch rpnStack {
    // Done processing — return all accumulated LCA candidates
    | list{} => ancestorsStack
    // Still elements to process
    | list{(depth, val, nodeKind), ...rest} =>
      switch (depth === maxDepth, nodeKind) {
      // Deepest leaf node: push onto evaluation stack
      | (true, Leaf) =>
        processRPNStack(ancestorsStack, list{val, ...evaluationStack}, maxDepth, rest)
      // Not possible path: skip
      | (true, Branch) => processRPNStack(ancestorsStack, evaluationStack, maxDepth, rest)
      // Non-deepest leaf: skip
      | (false, Leaf) => processRPNStack(ancestorsStack, evaluationStack, maxDepth, rest)
      // Possible ancestor of deepest leaves
      | (false, Branch) =>
        processRPNStack(
          // If it's one level above max depth, reduce the evaluation stack
          // and treat current node as an ancestor candidate
          depth === maxDepth - 1
            ? evaluationStack->List.reduce(list{val, ...ancestorsStack}, (acc, curr) => {
                list{curr, ...acc}
              })
            : ancestorsStack,
          list{}, // clear evaluation stack for next subtree
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
      processRPNStack(list{}, list{}, maxDepth, rpnStack)->List.toArray
    }
  }
}

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = Some(TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15)))
let r1 = lowestCommonAncestorOfDeepestLeaves(root1)
Console.log2("r1: ", r1) // [ 7, 3, 5, 18, 15 ]
