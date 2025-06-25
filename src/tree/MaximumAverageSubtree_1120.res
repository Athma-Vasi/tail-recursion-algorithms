// T(n) = O(n)
// S(n) = O(n)

type nodeKind = Leaf | Branch

let maximumAverageSubtree = (root: option<TreeNode.t<int>>) => {
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

  // Processes the RPN stack to compute the maximum average subtree value.
  // Uses an evaluation stack to simulate subtree aggregation and combine results.
  let rec processRPNStack = (
    rpnStack: list<(int, nodeKind)>, // Nodes remaining to process
    maxAverage: int, // Tracks the maximum average found so far
    evaluationStack: list<(int, nodeKind)>,
  ) => {
    // Stack used to aggregate subtree values

    switch rpnStack {
    // When RPN stack is empty, return the maximum average found
    | list{} => maxAverage
    // Process the next node in the RPN stack
    | list{(val, nodeKind), ...rest} =>
      switch nodeKind {
      // For leaf nodes, simply push them onto the evaluation stack
      | Leaf => processRPNStack(rest, maxAverage, list{(val, nodeKind), ...evaluationStack})
      // For branch nodes, aggregate values from up to two leaf subtrees on top of evaluation stack
      | Branch => {
          // Extract up to two leaf nodes (subtrees) from evaluation stack
          // 'topLeaves' holds the values of up to two leaf nodes
          // 'sliced' holds the remainder of the evaluation stack
          let (topLeaves, sliced) = evaluationStack->List.reduceWithIndex((list{}, list{}), (
            acc,
            curr,
            idx,
          ) => {
            let (topLeaves, sliced) = acc
            let (val, nodeKind) = curr

            // Collect up to two leaf nodes; others are moved to sliced
            idx < 2 && nodeKind === Leaf
              ? (list{val, ...topLeaves}, sliced)
              : (topLeaves, list{(val, nodeKind), ...sliced})
          })

          switch topLeaves {
          // No leaf subtrees found: consider current node value alone for max average
          | list{} => {
              let newMaxAverage = val > maxAverage ? val : maxAverage
              // Push the new max average as a Leaf node onto the evaluation stack and continue
              processRPNStack(rest, newMaxAverage, list{(newMaxAverage, Leaf), ...sliced})
            }
          // At least one leaf subtree found
          | list{topLeaf, ...restLeaves} => {
              // Calculate average for current node + one leaf subtree
              let avg = (val + topLeaf) / 2
              // Determine max among the single leaf value, average, and current maxAverage
              let newMaxAverage =
                [topLeaf, avg, maxAverage]->Array.reduce(-2, (max, curr) => max > curr ? max : curr)

              switch restLeaves {
              // Only one leaf subtree exists: update stack with new max average
              | list{} =>
                processRPNStack(rest, newMaxAverage, list{(newMaxAverage, Leaf), ...sliced})
              // Two leaf subtrees exist: compute average including both subtrees
              | list{nextTopLeaf, ..._nextRestLeaves} => {
                  let avg = (val + topLeaf + nextTopLeaf) / 3
                  // Determine max among the two leaf values, their combined average, and maxAverage
                  let newMaxAverage =
                    [topLeaf, nextTopLeaf, avg, maxAverage]->Array.reduce(-2, (max, curr) =>
                      max > curr ? max : curr
                    )

                  // Update stack and continue processing
                  processRPNStack(rest, newMaxAverage, list{(newMaxAverage, Leaf), ...sliced})
                }
              }
            }
          }
        }
      }
    }
  }

  switch root {
  // If the tree is empty, return -1 (no subtrees)
  | None => -1
  | Some(node) =>
    // 1. Perform postorder traversal to generate RPN list
    // 2. Process RPN stack to compute maximum average subtree
    postorderTraverse(list{}, list{node})->processRPNStack(-1, list{})
  }
}

/**
    5
   / \
  6   1
 */
let tree1 = Some(
  TreeNode.make(~val=5, ~left=Some(TreeNode.make(~val=6)), ~right=Some(TreeNode.make(~val=1))),
)
let r1 = maximumAverageSubtree(tree1)
Console.log2("r1: ", r1) // 6
