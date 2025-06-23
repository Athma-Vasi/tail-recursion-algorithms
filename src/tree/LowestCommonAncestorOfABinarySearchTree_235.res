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
  // Performs postorder traversal (left, right, root), storing values and node types
  // in an RPN-like stack format: each entry is a tuple (value, Leaf | Branch)
  let rec postorderTraverse = (
    rpnStack: list<(int, nodeKind)>, // Output RPN stack
    workingStack: list<TreeNode.t<int>>,
  ): // Traversal stack
  list<(int, nodeKind)> => {
    switch workingStack {
    // If traversal is complete, return the accumulated RPN stack
    | list{} => rpnStack
    // Continue traversal with the next node
    | list{node, ...rest} => {
        let {left, right, val} = node

        switch (left, right) {
        // Leaf node: push value with Leaf tag
        | (None, None) => postorderTraverse(list{(val, Leaf), ...rpnStack}, rest)

        // Only right child: push value with Branch tag, process right next
        | (None, Some(rightNode)) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{rightNode, ...rest})

        // Only left child: push value with Branch tag, process left next
        | (Some(leftNode), None) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{leftNode, ...rest})

        // Both children: push value with Branch tag, process left and right next
        | (Some(leftNode), Some(rightNode)) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{rightNode, leftNode, ...rest})
        }
      }
    }
  }

  // Process the RPN stack to evaluate and find the lowest common ancestor
  let rec processRPNStack = (
    lowestCommonAncestor: option<int>, // Currently tracked LCA
    evaluationStack: list<int>, // Simulates stack evaluation like in RPN
    rpnStack: list<(int, nodeKind)>,
  ): // Remaining RPN stack to process
  option<int> => {
    switch rpnStack {
    // Done processing: return the final LCA (if any)
    | list{} => lowestCommonAncestor

    // Process next node in RPN stack
    | list{(val, nodeKind), ...rest} =>
      switch nodeKind {
      // Leaf: push value onto evaluation stack
      | Leaf => processRPNStack(lowestCommonAncestor, list{val, ...evaluationStack}, rest)

      // Branch: simulate a "combine" operation in RPN by popping 2 values
      | Branch => {
          // Determine if this branch node is the LCA: it must combine both p and q
          let isLCA =
            List.size(evaluationStack) < 2
              ? false
              : evaluationStack
                ->List.reduce(list{}, (acc, num) => {
                  num === p || num === q ? list{true, ...acc} : acc
                })
                ->List.size === 2

          // Slice off the top two values from the evaluation stack (simulated pop)
          let sliced = evaluationStack->List.reduceWithIndex(list{}, (acc, curr, idx) => {
            idx === 0 || idx === 1 ? acc : list{curr, ...acc}
          })

          // Push current value onto evaluation stack, continue processing
          processRPNStack(isLCA ? Some(val) : lowestCommonAncestor, list{val, ...sliced}, rest)
        }
      }
    }
  }

  // Entry point: start postorder traversal and evaluation
  switch root {
  | None => None
  | Some(node) => processRPNStack(None, list{}, postorderTraverse(list{}, list{node}))
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
