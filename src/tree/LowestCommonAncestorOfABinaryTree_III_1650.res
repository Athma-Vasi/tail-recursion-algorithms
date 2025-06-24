// T(n) = O(n)
// S(n) = O(n)

// Finds the Lowest Common Ancestor (LCA) of two nodes `p` and `q` in a binary tree.
// It does this by finding the path from the root to `p` and from the root to `q`,
// and then identifying the last common node in both paths.
let lowestCommonAncestorOfABinaryTree_III = (root: option<TreeNode.t<int>>, p: int, q: int) => {
  // Performs an iterative preorder traversal to find the path from the root
  // to a given node `valueToFind`. Returns the path as a list of node values.
  let rec preorderTraverse = (
    pathToRoot: list<int>, // Unused if early exit occurs
    valueToFind: int,
    // Stack holds (node, path so far)
    stack: list<(TreeNode.t<int>, list<int>)>,
  ) => {
    switch stack {
    // No more nodes to explore: value not found
    | list{} => pathToRoot

    // Pop current node and continue traversal
    | list{(node, pathSoFar), ...rest} => {
        let {left, right, val} = node
        // Extend current path with this node's value
        let newPath = list{val, ...pathSoFar}

        switch val === valueToFind {
        // Found the target node: return the full path
        | true => newPath

        // Keep exploring children
        | false =>
          switch (left, right) {
          | (None, None) => preorderTraverse(pathToRoot, valueToFind, rest) // No children
          | (None, Some(rightNode)) =>
            preorderTraverse(pathToRoot, valueToFind, list{(rightNode, newPath), ...rest})
          | (Some(leftNode), None) =>
            preorderTraverse(pathToRoot, valueToFind, list{(leftNode, newPath), ...rest})
          | (Some(leftNode), Some(rightNode)) =>
            // Add both children to stack, right pushed after left (preorder-like)
            preorderTraverse(
              pathToRoot,
              valueToFind,
              list{(leftNode, newPath), (rightNode, newPath), ...rest},
            )
          }
        }
      }
    }
  }

  // Compares two paths from root to `p` and `q`, returning the last common value.
  let findLastMatchingElement = (pPath: list<int>, qPath: list<int>) => {
    let rec find = (stack: list<int>, index: int) =>
      switch (pPath->List.get(index), qPath->List.get(index)) {
      // Both paths have a node at this index
      | (Some(pVal), Some(qVal)) =>
        switch pVal === qVal {
        // Match: add to stack and continue
        | true => find(list{pVal, ...stack}, index + 1)
        // Diverged: return the last matched node
        | false => stack->List.head->Option.mapOr(Int32.min_int, v => v)
        }

      // One or both paths exhausted: return last matched
      | _ => stack->List.head->Option.mapOr(Int32.min_int, v => v)
      }

    find(list{}, 0)
  }

  // Entry point: handle root existence and kick off traversal
  switch root {
  | None => Int32.min_int // Invalid case
  | Some(node) => {
      // Find paths from root to p and q
      let pPath = preorderTraverse(list{}, p, list{(node, list{})})->List.reverse
      let qPath = preorderTraverse(list{}, q, list{(node, list{})})->List.reverse
      // Return the last common node in both paths
      findLastMatchingElement(pPath, qPath)
    }
  }
}

/**
        3
       / \
      5   1
     / \  / \
    6  2 0  8
      / \
     7   4
 */
let tree1 = Some(
  TreeNode.make(
    ~val=3,
    ~left=Some(
      TreeNode.make(
        ~val=5,
        ~left=Some(TreeNode.make(~val=6)),
        ~right=Some(
          TreeNode.make(
            ~val=2,
            ~left=Some(TreeNode.make(~val=7)),
            ~right=Some(TreeNode.make(~val=4)),
          ),
        ),
      ),
    ),
    ~right=Some(
      TreeNode.make(~val=1, ~left=Some(TreeNode.make(~val=0)), ~right=Some(TreeNode.make(~val=8))),
    ),
  ),
)
let r1 = lowestCommonAncestorOfABinaryTree_III(tree1, 5, 1)
Console.log2("r1: ", r1) // 3

/**
        3    
       / \
      5   1
     / \ / \
    6  2 0  8
      / \
     7   4
 */
let tree2 = Some(
  TreeNode.make(
    ~val=3,
    ~left=Some(
      TreeNode.make(
        ~val=5,
        ~left=Some(TreeNode.make(~val=6)),
        ~right=Some(
          TreeNode.make(
            ~val=2,
            ~left=Some(TreeNode.make(~val=7)),
            ~right=Some(TreeNode.make(~val=4)),
          ),
        ),
      ),
    ),
    ~right=Some(
      TreeNode.make(~val=1, ~left=Some(TreeNode.make(~val=0)), ~right=Some(TreeNode.make(~val=8))),
    ),
  ),
)

let r2 = lowestCommonAncestorOfABinaryTree_III(tree2, 5, 4)
Console.log2("r2: ", r2) // 5
