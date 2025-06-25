// T(n) = O(n)
// S(n) = O(n)

type nodeKind = Leaf | Branch

let countUnivalueSubtrees = (root: option<TreeNode.t<int>>) => {
  // Performs a postorder traversal that flattens the binary tree into a list.
  // Each element of the resulting list is a tuple:
  //   (node value, Leaf | Branch), where Branch implies the node had at least one child.
  let rec postorderTraverse = (
    rpnStack: list<(int, nodeKind)>, // Accumulator in reverse polish notation order
    // Stack of nodes to traverse
    workingStack: list<TreeNode.t<int>>,
  ) => {
    switch workingStack {
    | list{} => rpnStack // Traversal complete
    | list{top, ...rest} => {
        let {left, right, val} = top

        // Classify current node as Leaf or Branch, and process accordingly
        switch (left, right) {
        | (None, None) =>
          // Leaf node: record it and continue
          postorderTraverse(list{(val, Leaf), ...rpnStack}, rest)
        | (None, Some(rightNode)) =>
          // Branch with only right child: record, push right
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{rightNode, ...rest})
        | (Some(leftNode), None) =>
          // Branch with only left child: record, push left
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{leftNode, ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          // Branch with both children: push right then left for postorder simulation
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{rightNode, leftNode, ...rest})
        }
      }
    }
  }

  // Evaluates the RPN-style list to count univalue subtrees.
  // Each node is pushed back into the evaluationStack along with:
  //   - its value
  //   - its classification (Leaf or Branch)
  //   - cumulative univalue count up to that point
  let rec processRPNStack = (
    rpnStack: list<(int, nodeKind)>,
    univalueCount: int, // Running total of univalue subtrees
    leavesCount: int, // Number of consecutive leaves seen before this branch
    // Stack of evaluated nodes
    evaluationStack: list<(int, nodeKind, int)>,
  ) => {
    switch rpnStack {
    | list{} => univalueCount // Final result
    | list{(val, nodeKind), ...rest} =>
      switch nodeKind {
      | Leaf =>
        // Leaf is always a univalue subtree
        processRPNStack(
          rest,
          univalueCount,
          leavesCount + 1,
          list{(val, nodeKind, 1), ...evaluationStack},
        )

      | Branch => {
          // Pull up to `leavesCount` most recent leaf nodes from evaluation stack
          let (topLeaves, sliced) = evaluationStack->List.reduceWithIndex((list{}, list{}), (
            acc,
            curr,
            idx,
          ) => {
            let (topLeaves, sliced) = acc
            let (val, nodeKind, countSoFar) = curr

            idx < leavesCount && nodeKind === Leaf
              ? (list{(val, countSoFar), ...topLeaves}, sliced)
              : (topLeaves, list{(val, nodeKind, countSoFar), ...sliced})
          })

          // Fold collected leaves to compute combined value list and count
          let (leavesValues, countWithLeaves) = topLeaves->List.reduce((list{}, univalueCount), (
            acc,
            (currVal, countSoFar),
          ) => {
            let (leavesValues, countWithLeaves) = acc
            ({list{currVal, ...leavesValues}}, countWithLeaves + countSoFar)
          })

          // Determine if current branch is a univalue subtree
          let newCount = switch leavesValues {
          | list{} => countWithLeaves
          | list{topLeaf, ...restLeaves} =>
            switch restLeaves {
            | list{} =>
              // Only one leaf: check if it matches the branch root
              val === topLeaf ? countWithLeaves + 1 : countWithLeaves
            | list{nextLeaf, ..._nextRestLeaves} =>
              // Two or more leaves: all must equal the branch root
              val === topLeaf && val === nextLeaf ? countWithLeaves + 1 : countWithLeaves
            }
          }

          // Push current branch back into evaluation stack
          processRPNStack(rest, newCount, 0, list{(val, Leaf, newCount), ...sliced})
        }
      }
    }
  }

  // Entry point
  switch root {
  | None => -1 // Invalid tree
  | Some(node) =>
    // Step 1: Flatten the tree into RPN order
    // Step 2: Process it to count univalue subtrees
    postorderTraverse(list{}, list{node})->processRPNStack(0, 0, list{})
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
