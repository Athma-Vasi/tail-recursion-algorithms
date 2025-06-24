// T(n) = O(n)
// S(n) = O(h)

// Computes the distance (number of edges) between two nodes p and q in a binary tree.
let findDistanceInABinaryTree = (root: option<TreeNode.t<int>>, p: int, q: int) => {
  // --------------------------------------------------------------------------
  // Helper: Preorder traversal to find the path from root to a target value.
  // Returns Some(path) once found (in reverse), else None.
  //
  // pathToRoot : option<list<int>>  — Some(pathSoFar) to short-circuit once found
  // valueToFind : int               — target node value
  // stack : list<(node, pathSoFar)>  — explicit stack for traversal
  // --------------------------------------------------------------------------
  let rec preorderTraverse = (
    pathToRoot: option<list<int>>,
    valueToFind: int,
    stack: list<(TreeNode.t<int>, list<int>)>,
  ) => {
    switch pathToRoot {
    // If we already found the target, reverse and return the stored path
    | Some(path) => path->List.reverse

    | None =>
      switch stack {
      // No nodes left => target not found => return empty path
      | list{} => list{}

      // Pop the next node off the stack
      | list{(node, pathSoFar), ...rest} => {
          let {left, right, val} = node
          // Build the new path by prepending this node’s value
          let newPath = list{val, ...pathSoFar}

          switch (val === valueToFind, left, right) {
          // Found the target: mark pathToRoot to short-circuit
          | (true, _, _) => preorderTraverse(Some(newPath), valueToFind, rest)

          // Leaf with no children: continue with rest
          | (false, None, None) => preorderTraverse(None, valueToFind, rest)

          // Only right child: push it onto the stack
          | (false, None, Some(rightNode)) =>
            preorderTraverse(None, valueToFind, list{(rightNode, newPath), ...rest})

          // Only left child: push it onto the stack
          | (false, Some(leftNode), None) =>
            preorderTraverse(None, valueToFind, list{(leftNode, newPath), ...rest})

          // Both children: push left then right to explore left first
          | (false, Some(leftNode), Some(rightNode)) =>
            preorderTraverse(
              None,
              valueToFind,
              list{(leftNode, newPath), (rightNode, newPath), ...rest},
            )
          }
        }
      }
    }
  }

  // --------------------------------------------------------------------------
  // Helper: Given a list of paths (each path is list of node values from root),
  // find the deepest (last) common value among all paths.
  // --------------------------------------------------------------------------
  let findLastMatchingElement = (paths: list<list<int>>) => {
    let rec travelPaths = (matches: list<int>, nodeIdx: int) => {
      // Build a set of all node values at this index in each path
      let matchSet = paths->List.reduce(Set.make(), (acc, path) => {
        switch path->List.get(nodeIdx) {
        | None => {
            // Mismatch if any path is too short: add dummy duplicates
            acc->Set.add(0)
            acc->Set.add(0)
            acc
          }
        | Some(nodeVal) => {
            acc->Set.add(nodeVal)
            acc
          }
        }
      })

      // If more than one distinct value, we’ve diverged
      switch Set.size(matchSet) === 1 {
      | false =>
        // Return the last matching node (head of matches list)
        matches->List.head->Option.getOr(Int32.min_int)

      | true =>
        // All paths share the same value here: record it and continue deeper
        let commonVal =
          matchSet
          ->Set.values
          ->Array.fromIterator
          ->Array.at(0)
          ->Option.getOr(Int32.min_int)
        travelPaths(list{commonVal, ...matches}, nodeIdx + 1)
      }
    }

    // Start comparison from depth 0 with an empty matches list
    travelPaths(list{}, 0)
  }

  // --------------------------------------------------------------------------
  // Main entrypoint: handle trivial cases, then compute distance via:
  //   dist(p, q) = dist(root, p) + dist(root, q) - 2 * dist(root, LCA)
  // where dist(root, x) = length of path from root to x (in nodes).
  // Subtract 2* because edges = nodes - 1.
  // --------------------------------------------------------------------------
  switch (root, p === q) {
  // No tree or same node requested => distance 0 (or invalid tree)
  | (None, true) => 0
  | (None, false) => Int32.min_int
  | (Some(_), true) => 0

  // General case: find paths, LCA, then compute distance
  | (Some(node), false) =>
    // 1. Path from root to p, and to q
    let pPath = preorderTraverse(None, p, list{(node, list{})})
    let qPath = preorderTraverse(None, q, list{(node, list{})})

    // 2. Find lowest common ancestor value
    let lcaVal = findLastMatchingElement(list{pPath, qPath})

    // 3. Compute path to LCA
    let lcaPath = preorderTraverse(None, lcaVal, list{(node, list{})})

    // 4. Distance in edges:
    //    (#nodes on pPath) + (#nodes on qPath) - 2*(#nodes on lcaPath)
    List.size(pPath) + List.size(qPath) - 2 * List.size(lcaPath)
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

let r1 = findDistanceInABinaryTree(tree, 5, 0)
Console.log2("r1: ", r1) // 3

let r2 = findDistanceInABinaryTree(tree, 5, 7)
Console.log2("r2: ", r2) // 2

let r3 = findDistanceInABinaryTree(tree, 5, 5)
Console.log2("r3: ", r3) // 0
