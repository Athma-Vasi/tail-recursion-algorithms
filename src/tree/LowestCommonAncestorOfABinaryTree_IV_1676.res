// T(n) = O(k·n + k·h)
// Where: n is the number of nodes in the tree
// k is the number of target nodes in nodes
// h is the height of the tree (bounded by n)
// S(n) = O(k·h)

let lowestCommonAncestorOfABinaryTree_IV = (root: option<TreeNode.t<int>>, nodes: array<int>) => {
  // Traverses the binary tree in preorder (root, left, right) to find the path
  // from the root to a specific target value. If found, returns the path as a list of node values.
  let rec preorderTraverse = (
    pathToRoot: option<list<int>>, // Accumulates and returns early when the target is found
    valueToFind: int,
    stack: list<(TreeNode.t<int>, list<int>)>,
  ) => {
    // Stack of (currentNode, pathSoFar)

    switch pathToRoot {
    | Some(path) => path->List.reverse // If found, return reversed path (from root to target)
    | None =>
      switch stack {
      | list{} => list{} // Target not found
      | list{(node, pathSoFar), ...rest} => {
          let {left, right, val} = node
          let newPath = list{val, ...pathSoFar} // Append current node to path

          switch (val === valueToFind, left, right) {
          | (true, _, _) =>
            // Found the target node; store the path
            preorderTraverse(Some(newPath), valueToFind, rest)

          | (false, None, None) =>
            // No children to explore; continue with rest of stack
            preorderTraverse(pathToRoot, valueToFind, rest)

          | (false, None, Some(rightNode)) =>
            // Only right child exists
            preorderTraverse(pathToRoot, valueToFind, list{(rightNode, newPath), ...rest})

          | (false, Some(leftNode), None) =>
            // Only left child exists
            preorderTraverse(pathToRoot, valueToFind, list{(leftNode, newPath), ...rest})

          | (false, Some(leftNode), Some(rightNode)) =>
            // Both children exist; add both to stack
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

  // Finds the deepest common node value shared among all given paths.
  let findLastMatchingElement = (paths: list<list<int>>) => {
    let rec travelPaths = (matches: list<int>, nodeIdx: int) => {
      // Create a set of all node values at current index across all paths
      let matchSet = paths->List.reduce(Set.make(), (acc, path) => {
        switch path->List.get(nodeIdx) {
        | None => {
            // Force mismatch by adding duplicate
            acc->Set.add(0)
            acc->Set.add(0)
            acc
          }
        | Some(node) => {
            acc->Set.add(node)
            acc
          }
        }
      })

      switch Set.size(matchSet) === 1 {
      | false =>
        // Nodes diverge at this index; return last common node
        matches->List.head->Option.getOr(Int32.min_int)
      | true => {
          // All nodes share the same value at this index; continue
          let match =
            matchSet
            ->Set.values
            ->Array.fromIterator
            ->Array.at(0)
            ->Option.getOr(Int32.min_int)

          travelPaths(list{match, ...matches}, nodeIdx + 1)
        }
      }
    }

    travelPaths(list{}, 0)
  }

  // Handle edge cases and run traversal
  switch (root, nodes->Array.length) {
  | (None, _) => Int32.min_int // No root node
  | (Some(_node), 0) => Int32.min_int // No target nodes
  | (Some(_), 1) =>
    // Only one target node; it is the LCA of itself
    nodes->Array.get(0)->Option.getOr(Int32.min_int)
  | (Some(node), _) =>
    // Get root-to-node paths for all nodes, then find common ancestor
    nodes
    ->Array.reduceRight(list{}, (acc, curr) => {
      let path = preorderTraverse(None, curr, list{(node, list{})})
      list{path, ...acc}
    })
    ->findLastMatchingElement
  }
}

/**
        3    
       / \
      5   1
     / \ / \
    6  2 0  8
      / \
     7   4
 */
let tree = Some(
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
let r1 = lowestCommonAncestorOfABinaryTree_IV(tree, [4, 7])
Console.log2("r1: ", r1) // 2

let r2 = lowestCommonAncestorOfABinaryTree_IV(tree, [1])
Console.log2("r2: ", r2) // 1

let r3 = lowestCommonAncestorOfABinaryTree_IV(tree, [7, 6, 2, 4])
Console.log2("r3: ", r3) // 5

let r4 = lowestCommonAncestorOfABinaryTree_IV(tree, [0, 1, 2, 3, 4, 5, 6, 7, 8])
Console.log2("r4: ", r4) // 3
