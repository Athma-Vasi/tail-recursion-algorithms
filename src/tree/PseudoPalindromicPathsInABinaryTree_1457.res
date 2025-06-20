// T(n) = O(n)
// S(n) = O(n)

// Determines if there exists a root-to-leaf path in the binary tree
// where the digits can be rearranged into a palindrome (i.e., pseudo-palindromic).
let pseudoPalindromicPathsInABinaryTree = (root: option<TreeNode.t<int>>) => {
  // Recursive DFS using a manual stack to collect all root-to-leaf paths
  let rec preorderTraverse = (
    paths: list<list<int>>, // Accumulator for complete root-to-leaf paths
    // Stack of (node, pathSoFar)
    stack: list<(TreeNode.t<int>, list<int>)>,
  ) => {
    switch stack {
    | list{} => paths // If the stack is empty, return the accumulated paths
    | list{(node, pathSoFar), ...rest} => {
        let {left, right, val} = node
        // Prepend current node's value to the path
        let newPath = list{val, ...pathSoFar}

        switch (left, right) {
        | (None, None) =>
          // If the node is a leaf, add the full path to the list of completed paths
          preorderTraverse(list{newPath, ...paths}, rest)
        | (None, Some(rightNode)) =>
          // Only right child exists; continue traversal with updated path
          preorderTraverse(paths, list{(rightNode, newPath), ...rest})
        | (Some(leftNode), None) =>
          // Only left child exists; continue traversal with updated path
          preorderTraverse(paths, list{(leftNode, newPath), ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          // Both children exist; push both onto the stack with the same path so far
          preorderTraverse(paths, list{(leftNode, newPath), (rightNode, newPath), ...rest})
        }
      }
    }
  }

  switch root {
  | None => false // No tree, no paths
  | Some(node) =>
    preorderTraverse(list{}, list{(node, list{})}) // Start traversal with empty path and root node
    ->List.reverse // Reverse final list to get original path order
    ->List.toArray // Convert list to array for easier reduction
    ->Array.reduce(Set.make(), (acc, paths) => {
      // Build a frequency table of digits for this path
      let freqTable =
        paths
        ->List.reverse // Reverse path back to root-to-leaf order
        ->List.toArray
        ->Array.reduce(Map.make(), (mapAcc, path) => {
          // For each digit, update its count in the frequency map
          let freq = mapAcc->Map.get(path)->Option.mapOr(1, f => f + 1)
          mapAcc->Map.set(path, freq)
          mapAcc
        })

      // Check if the path is pseudo-palindromic
      let isPathPseudoPalindromic =
        freqTable
        ->Map.values // Get all digit frequencies
        ->Array.fromIterator
        ->Array.reduce(0, (countAcc, count) => {
          // Count how many digits have odd frequencies
          Float.mod(Int.toFloat(count), 2.0) === 0.0 ? countAcc : countAcc + 1
        }) <= 1 // Path is pseudo-palindromic if ≤ 1 digit has odd frequency

      acc->Set.add(isPathPseudoPalindromic) // Record result for this path
      acc
    })
    ->Set.has(true) // Final result: true if any path was pseudo-palindromic
  }
}

/**
        2
       / \
      3   1
     / \    \
    3   1    1
 */
let tree: option<TreeNode.t<int>> = Some(
  TreeNode.make(
    ~val=2,
    ~left=Some(
      TreeNode.make(~val=3, ~left=Some(TreeNode.make(~val=3)), ~right=Some(TreeNode.make(~val=1))),
    ),
    ~right=Some(TreeNode.make(~val=1, ~right=Some(TreeNode.make(~val=1)))),
  ),
)
let r1 = pseudoPalindromicPathsInABinaryTree(tree)
Console.log2("r1: ", r1) // true

/**
        10
       /  \
      5    15
     / \     \
    3   7     18
 */
let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = Some(TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15)))
let r2 = pseudoPalindromicPathsInABinaryTree(root1)
Console.log2("r2: ", r2) // false
