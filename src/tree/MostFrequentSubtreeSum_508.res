// T(n) = O(n)
// S(n) = O(n)

type nodeKind = Leaf | Branch

// Computes the most frequent subtree sum(s) in a binary tree.
// A subtree sum is the sum of all values in a node's subtree, including the node itself.
let mostFrequentSubtreeSum = (root: option<TreeNode.t<int>>) => {
  // Postorder traversal that collects nodes in reverse polish notation (RPN) format,
  // recording both the value and whether the node is a leaf or branch.
  let rec postorderTraverse = (
    rpnStack: list<(int, nodeKind)>, // Accumulates node values in postorder
    workingStack: list<TreeNode.t<int>>,
  ): // Stack for manual tree traversal
  list<(int, nodeKind)> => {
    switch workingStack {
    // Traversal is done
    | list{} => rpnStack

    // Process the next node
    | list{node, ...rest} => {
        let {left, right, val} = node

        switch (left, right) {
        // Leaf node: just push its value with Leaf tag
        | (None, None) => postorderTraverse(list{(val, Leaf), ...rpnStack}, rest)

        // Only right child: tag as Branch and push right next
        | (None, Some(rightNode)) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{rightNode, ...rest})

        // Only left child: tag as Branch and push left next
        | (Some(leftNode), None) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{leftNode, ...rest})

        // Both children: tag as Branch and push both in order (left first for postorder)
        | (Some(leftNode), Some(rightNode)) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{leftNode, rightNode, ...rest})
        }
      }
    }
  }

  // Processes the RPN stack to compute actual subtree sums,
  // and collects their frequencies in a map.
  let rec processRPNStack = (
    rpnStack: list<(int, nodeKind)>, // Remaining RPN input
    freqTable: Map.t<int, int>, // Map to record frequency of each sum
    evaluationStack: list<int>,
  ) => {
    // Simulates a value stack for subtree computation

    switch rpnStack {
    // All RPN nodes processed — return frequency table
    | list{} => freqTable

    // Process next node
    | list{(val, nodeKind), ...rest} =>
      switch nodeKind {
      // Leaf: its value is the subtree sum; update frequency
      | Leaf => {
          let freq = freqTable->Map.get(val)->Option.mapOr(1, f => f + 1)
          freqTable->Map.set(val, freq)

          processRPNStack(rest, freqTable, list{val, ...evaluationStack})
        }

      // Branch: simulate RPN evaluation by combining top values
      | Branch => {
          // Compute sum of this branch: left + right + current node value
          let subtreeSum = evaluationStack->List.reduce(0, (acc, num) => acc + num) + val

          // Update frequency table
          let freq = freqTable->Map.get(subtreeSum)->Option.mapOr(1, f => f + 1)
          freqTable->Map.set(subtreeSum, freq)

          // Simulate removing the top 2 values (left & right children),
          // and replacing them with the subtree sum
          let sliced = evaluationStack->List.reduceWithIndex(list{}, (acc, curr, idx) => {
            idx === 0 || idx === 1 ? acc : list{curr, ...acc}
          })

          processRPNStack(rest, freqTable, list{subtreeSum, ...sliced})
        }
      }
    }
  }

  // Entry point
  switch root {
  | None => [] // No tree: return empty array
  | Some(node) => {
      // Step 1: Create postorder RPN stack
      let valFreqTuples =
        postorderTraverse(list{}, list{node})
        ->processRPNStack(Map.make(), list{})
        ->Map.entries // Get all (sum, freq) pairs
        ->Array.fromIterator

      // Step 2: Find max frequency
      let maxFreq = valFreqTuples->Array.reduce(0, (acc, (_val, freq)) => {
        freq > acc ? freq : acc
      })

      // Step 3: Collect all sums with max frequency
      valFreqTuples
      ->Array.reduce(list{}, (acc, (val, freq)) => {
        freq === maxFreq ? list{val, ...acc} : acc
      })
      ->List.toArray
    }
  }
}

/**
      5
     / \
    2  -3
 */
let tree1: option<TreeNode.t<int>> = Some(
  TreeNode.make(~val=5, ~left=Some(TreeNode.make(~val=2)), ~right=Some(TreeNode.make(~val=-3))),
)
let r1 = mostFrequentSubtreeSum(tree1)
Console.log2("r1: ", r1) // [4, 2, -3]

/**
      5
     / \
    2  -5
 */
let tree2: option<TreeNode.t<int>> = Some(
  TreeNode.make(~val=5, ~left=Some(TreeNode.make(~val=2)), ~right=Some(TreeNode.make(~val=-5))),
)
let r2 = mostFrequentSubtreeSum(tree2)
Console.log2("r2: ", r2) // [2]
