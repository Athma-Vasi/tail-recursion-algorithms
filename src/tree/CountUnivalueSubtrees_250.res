type nodeKind = Leaf | Branch

let countUnivalueSubtrees = (root: option<TreeNode.t<int>>) => {
  // Performs a postorder traversal of the binary tree.
  // Returns a list representing nodes in reverse polish notation (RPN) style.
  // Each element in the list is a tuple of (node value, node kind),
  // where node kind indicates whether the node is a Leaf or a Branch.
  let rec postorderTraverse = (
    rpnStack: list<(int, nodeKind)>, // Accumulates nodes in RPN order
    // Stack to manage traversal process
    workingStack: list<TreeNode.t<int>>,
  ) => {
    switch workingStack {
    // When no nodes are left to process, return the accumulated RPN stack
    | list{} => rpnStack
    // Process the current top node from the working stack
    | list{top, ...rest} => {
        let {left, right, val} = top

        // Identify if current node is a Leaf (no children) or Branch (has children)
        // and continue the traversal accordingly.
        switch (left, right) {
        // Leaf node: add to RPN stack and continue with remaining nodes
        | (None, None) => postorderTraverse(list{(val, Leaf), ...rpnStack}, rest)
        // Branch node with only right child: add current node and push right child to stack
        | (None, Some(rightNode)) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{rightNode, ...rest})
        // Branch node with only left child: add current node and push left child to stack
        | (Some(leftNode), None) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{leftNode, ...rest})
        // Branch node with two children:
        // push current node, then push right and left child nodes onto stack.
        // Order ensures left subtree is processed after right (postorder).
        | (Some(leftNode), Some(rightNode)) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{rightNode, leftNode, ...rest})
        }
      }
    }
  }

  let rec processRPNStack = (
    rpnStack: list<(int, nodeKind)>,
    univalueCount: int,
    // helps determine how much to slice in evaluationStack
    leavesCount: int,
    // (val, nodeKind, countSoFar)
    evaluationStack: list<(int, nodeKind, int)>,
  ) => {
    switch rpnStack {
    | list{} => univalueCount
    | list{(val, nodeKind), ...rest} =>
      switch nodeKind {
      | Leaf =>
        processRPNStack(
          rest,
          univalueCount,
          leavesCount + 1,
          list{(val, nodeKind, 1), ...evaluationStack},
        )
      | Branch => {
          let (topLeaves, sliced) = evaluationStack->List.reduceWithIndex((list{}, list{}), (
            acc,
            curr,
            idx,
          ) => {
            let (topLeaves, sliced) = acc
            let (val, nodeKind, countSoFar) = curr

            // Collect up to leavesCount leaf nodes; others are moved to sliced
            idx < leavesCount && nodeKind === Leaf
              ? (list{(val, countSoFar), ...topLeaves}, sliced)
              : (topLeaves, list{(val, nodeKind, countSoFar), ...sliced})
          })

          let (univalueChecks, withLeaves) = topLeaves->List.reduce((Set.make(), univalueCount), (
            acc,
            (currVal, countSoFar),
          ) => {
            let (univalueChecks, withLeaves) = acc
            univalueChecks->Set.add(currVal === val)
            (univalueChecks, withLeaves + countSoFar)
          })
          let newCount = univalueChecks->Set.has(false) ? withLeaves : withLeaves + 1

          Console.log("\n")
          Console.log2("topLeaves", topLeaves->List.toArray)
          Console.log2("sliced", sliced->List.toArray)
          Console.log2("univalueChecks", univalueChecks)
          Console.log2("withLeaves", withLeaves)
          Console.log2("newCount", newCount)
          Console.log2("rest", rest->List.toArray)

          processRPNStack(rest, newCount, 0, list{(val, Leaf, newCount), ...sliced})
        }
      }
    }
  }

  switch root {
  | None => -1
  | Some(node) => postorderTraverse(list{}, list{node})->processRPNStack(0, 0, list{})
  }
}

/**
              5
             / \
            1   5
           / \   \
          5   5   5
 */
let tree1 = Some(
  TreeNode.make(
    ~val=5,
    ~left=Some(
      TreeNode.make(~val=1, ~left=Some(TreeNode.make(~val=5)), ~right=Some(TreeNode.make(~val=5))),
    ),
    ~right=Some(TreeNode.make(~val=5, ~right=Some(TreeNode.make(~val=5)))),
  ),
)
let r1 = countUnivalueSubtrees(tree1)
Console.log2("r1: ", r1) // 4
