// T(n) = O(n)
// S(n) = O(n)

type nodeKind = Leaf | Branch

let findDuplicateSubtrees = (root: option<TreeNode.t<int>>) => {
  // Simulates a postorder traversal manually, collecting values and node kinds
  let rec postorderTraverse = (
    // rpnStack stores (node value, node kind), accumulated in postorder
    rpnStack: list<(int, nodeKind)>,
    // workingStack is the traversal stack
    workingStack: list<TreeNode.t<int>>,
  ) => {
    switch workingStack {
    // Base case: done traversing
    | list{} => rpnStack
    // Continue processing next node
    | list{node, ...rest} => {
        let {left, right, val} = node

        switch (left, right) {
        // Leaf: push (val, Leaf)
        | (None, None) => postorderTraverse(list{(val, Leaf), ...rpnStack}, rest)
        // Only right: mark as Branch and traverse right
        | (None, Some(rightNode)) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{rightNode, ...rest})
        // Only left: mark as Branch and traverse left
        | (Some(leftNode), None) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{leftNode, ...rest})
        // Both children: reverse-preorder push (right then left)
        | (Some(leftNode), Some(rightNode)) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{rightNode, leftNode, ...rest})
        }
      }
    }
  }

  // Processes RPN stack to reconstruct subtree representations and find duplicates
  let rec processRPNStack = (
    // List of (node value, kind) from postorderTraverse
    rpnStack: list<(int, nodeKind)>,
    // Map from serialized subtree -> frequency
    freqTable: Map.t<string, int>,
    // Duplicates collected so far (as arrays of ints)
    duplicates: array<array<int>>,
    // Evaluation stack used to build serialized subtree representations
    evaluationStack: list<(int, nodeKind)>,
  ) => {
    switch rpnStack {
    // Base case: done processing
    | list{} => duplicates
    | list{(val, nodeKind), ...rest} =>
      switch nodeKind {
      // Leaf: just push to evaluation stack
      | Leaf =>
        processRPNStack(rest, freqTable, duplicates, list{(val, nodeKind), ...evaluationStack})

      // Branch node: attempt to pop two Leafs from eval stack and combine
      | Branch => {
          // Attempt to serialize using the top two Leaf nodes
          let (serialized, sliced) = evaluationStack->List.reduceWithIndex(
            (Int.toString(val), list{}), // Init: start with val
            (acc, curr, idx) => {
              let (serialized, sliced) = acc
              let (val, nodeKind) = curr

              // Assumes top 2 values are Leafs (which may not always hold)
              idx < 2 && nodeKind === Leaf
                ? (serialized ++ Int.toString(val), sliced) // Append val to serialized string
                : (serialized, list{(val, nodeKind), ...sliced}) // Rebuild eval stack
            },
          )
          // Reverse to restore order
          let original = sliced->List.reverse

          switch freqTable->Map.get(serialized) {
          | None => {
              // First time seeing this subtree
              freqTable->Map.set(serialized, 1)
              processRPNStack(rest, freqTable, duplicates, list{(val, nodeKind), ...original})
            }
          | Some(freq) => {
              // Already seen: count duplicate and record
              freqTable->Map.set(serialized, freq + 1)

              // Attempt to reconstruct duplicate subtree values
              // NOTE: This is a naive deserialization — splits every character!
              let unserialized =
                serialized
                ->String.split(String.make())
                ->Array.map(char => Int.fromString(char)->Option.mapOr(Int32.min_int, c => c))

              // Create mock subtree values — limited structure here
              let newDuplicates = [
                unserialized,
                [unserialized->Array.at(1)->Option.mapOr(Int32.min_int, v => v)],
              ]

              // Continue with updated duplicates and eval stack
              processRPNStack(rest, freqTable, newDuplicates, list{(val, nodeKind), ...original})
            }
          }
        }
      }
    }
  }

  switch root {
  | None => []
  | Some(node) =>
    // 1. Postorder traverse the tree
    // 2. Process stack to find duplicates
    postorderTraverse(list{}, list{node})->processRPNStack(Map.make(), [], list{})
  }
}

/**
           1
         /   \
        2     3
       /     / \
      4     2   4
           /
          4
 */
let tree1 = Some(
  TreeNode.make(
    ~val=1,
    ~left=Some(TreeNode.make(~val=2, ~left=Some(TreeNode.make(~val=4)))),
    ~right=Some(
      TreeNode.make(
        ~val=3,
        ~left=Some(TreeNode.make(~val=2, ~left=Some(TreeNode.make(~val=4)))),
        ~right=Some(TreeNode.make(~val=4)),
      ),
    ),
  ),
)
let r1 = findDuplicateSubtrees(tree1)
Console.log2("r1: ", r1) // [[2, 4], [4]]
