// T(n) = O(n)
// S(n) = O(n)

type nodeKind = Leaf | Branch

// Find the lowest common ancestor (LCA) of two nodes in a Binary Search Tree (BST)
// using a custom postorder traversal and reverse polish notation (RPN) evaluation.
let lowestCommonAncestorOfABinarySearchTree = (
  root: option<TreeNode.t<int>>,
  p: int,
  q: int,
): option<int> => {
  // Performs a postorder traversal (left, right, root), storing each node as a tuple of:
  //   - its value
  //   - a tag indicating whether it's a Leaf or Branch
  // The result is a reverse-polish-notation-style stack used later to simulate evaluation.
  let rec postorderTraverse = (
    rpnStack: list<(int, nodeKind)>, // Accumulator for RPN output
    // Stack used to simulate postorder traversal
    workingStack: list<TreeNode.t<int>>,
  ): list<(int, nodeKind)> => {
    switch workingStack {
    // Traversal complete
    | list{} => rpnStack
    // Process next node in the stack
    | list{node, ...rest} => {
        let {left, right, val} = node

        switch (left, right) {
        // Leaf node: no children
        | (None, None) => postorderTraverse(list{(val, Leaf), ...rpnStack}, rest)

        // Only right child exists: treat current node as a branch and traverse right
        | (None, Some(rightNode)) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{rightNode, ...rest})

        // Only left child exists: treat current node as a branch and traverse left
        | (Some(leftNode), None) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{leftNode, ...rest})

        // Both children exist: simulate postorder by pushing right before left
        | (Some(leftNode), Some(rightNode)) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{rightNode, leftNode, ...rest})
        }
      }
    }
  }

  let rec processRPNStack = (
    rpnStack: list<(int, nodeKind)>, // Nodes in postorder (RPN) format
    lowestCommonAncestor: option<int>, // Result accumulator
    // Stack simulating subtree evaluation
    evaluationStack: list<(int, nodeKind)>,
  ): option<int> => {
    switch rpnStack {
    // All nodes processed; return final result
    | list{} => lowestCommonAncestor

    // Continue processing next node from the RPN stack
    | list{(val, nodeKind), ...rest} =>
      switch nodeKind {
      // Leaf node: push directly to evaluation stack
      | Leaf =>
        processRPNStack(rest, lowestCommonAncestor, list{(val, nodeKind), ...evaluationStack})

      // Branch node: try to determine if it's the LCA of p and q
      | Branch => {
          // Pull up to two most recent leaves from the evaluation stack
          // All other nodes are retained and reversed to maintain stack order
          let (topLeaves, sliced) = evaluationStack->List.reduceWithIndex((list{}, list{}), (
            acc,
            curr,
            idx,
          ) => {
            let (topLeaves, sliced) = acc
            let (val, nodeKind) = curr

            switch (idx < 2, nodeKind) {
            | (true, Leaf) => (list{(val, nodeKind), ...topLeaves}, sliced)
            | (true, Branch) => (topLeaves, list{curr, ...sliced})
            | (false, Leaf) => (topLeaves, list{curr, ...sliced})
            | (false, Branch) => (topLeaves, list{curr, ...sliced})
            }
          })

          // Restore original order of remaining evaluation stack
          let original = sliced->List.reverse

          // A node is considered the LCA if its top two leaves contain both p and q
          let isLCA =
            List.size(topLeaves) < 2
              ? false
              : topLeaves
                ->List.reduce(list{}, (acc, (val, _nodeKind)) => {
                  val === p || val === q ? list{true, ...acc} : acc
                })
                ->List.size === 2

          // Push current node back to evaluation stack as a Leaf for future evaluations
          processRPNStack(
            rest,
            isLCA ? Some(val) : lowestCommonAncestor,
            list{(val, Leaf), ...original},
          )
        }
      }
    }
  }

  // Entry point: start traversal and evaluation
  switch root {
  | None => None
  | Some(node) => postorderTraverse(list{}, list{node})->processRPNStack(None, list{})
  }
}

/**
          6
        /   \
       2     8
      / \   / \
     0   4 7   9
        / \
       3   5
 */
let tree: option<TreeNode.t<int>> = Some(
  TreeNode.make(
    ~val=6,
    ~left=Some(
      TreeNode.make(
        ~val=2,
        ~left=Some(TreeNode.make(~val=0)),
        ~right=Some(
          TreeNode.make(
            ~val=4,
            ~left=Some(TreeNode.make(~val=3)),
            ~right=Some(TreeNode.make(~val=5)),
          ),
        ),
      ),
    ),
    ~right=Some(
      TreeNode.make(~val=8, ~left=Some(TreeNode.make(~val=7)), ~right=Some(TreeNode.make(~val=9))),
    ),
  ),
)
let r1 = lowestCommonAncestorOfABinarySearchTree(tree, 2, 8)
Console.log2("r1: ", r1) // 6

// for binary search trees:
// let rec traverse = (lca: option<int>, curr: option<TreeNode.t<int>>) => {
//   switch curr {
//   | None => lca
//   | Some(node) => {
//       let {left, right, val} = node
//       switch (p < val, q < val, p > val, q > val) {
//       // both smaller, go left
//       | (true, true, _, _) => traverse(lca, left)
//       // both greater, go right
//       | (_, _, true, true) => traverse(lca, right)
//       // diverge: found lowest common ancestor
//       | _ => traverse(Some(val), None)
//       }
//     }
//   }
// }
// traverse(None, root)
