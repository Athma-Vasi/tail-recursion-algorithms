// T(n) = O(n)
// S(n) = O(n)
// INCORRECT
// does not work for first case

type state = Collecting | Finished

let boundaryOfBinaryTree = (root: option<TreeNode.t<int>>) => {
  // Collects all leaf nodes in preorder fashion
  let rec collectLeaves = (leaves: list<int>, stack: list<TreeNode.t<int>>) => {
    switch stack {
    | list{} => leaves->List.reverse // Traversal complete
    | list{node, ...rest} => {
        let {left, right, val} = node

        switch (left, right) {
        // Node is a leaf: include its value
        | (None, None) => collectLeaves(list{val, ...leaves}, rest)

        // Only right child exists
        | (None, Some(rightNode)) => collectLeaves(leaves, list{rightNode, ...rest})

        // Only left child exists
        | (Some(leftNode), None) => collectLeaves(leaves, list{leftNode, ...rest})

        // Both children exist: push both, maintaining left-to-right order
        | (Some(leftNode), Some(rightNode)) =>
          collectLeaves(leaves, list{leftNode, rightNode, ...rest})
        }
      }
    }
  }

  let rec leftSideView = (
    levelLeftmostTable: Map.t<int, int>,
    stack: list<(TreeNode.t<int>, int)>,
  ) => {
    switch stack {
    | list{} =>
      levelLeftmostTable
      ->Map.entries
      ->Array.fromIterator
      ->Array.reduceRight(list{}, (acc, (_level, leftmost)) => list{leftmost, ...acc})
    | list{(node, level), ...rest} => {
        let {left, right, val} = node
        levelLeftmostTable->Map.set(level, val)

        switch (left, right) {
        | (None, None) => leftSideView(levelLeftmostTable, rest)
        | (None, Some(rightNode)) =>
          leftSideView(levelLeftmostTable, list{(rightNode, level + 1), ...rest})
        | (Some(leftNode), None) =>
          leftSideView(levelLeftmostTable, list{(leftNode, level + 1), ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          leftSideView(
            levelLeftmostTable,
            list{(rightNode, level + 1), (leftNode, level + 1), ...rest},
          )
        }
      }
    }
  }

  let rec rightSideView = (
    levelRightmostTable: Map.t<int, int>,
    stack: list<(TreeNode.t<int>, int)>,
  ) => {
    switch stack {
    | list{} =>
      levelRightmostTable
      ->Map.entries
      ->Array.fromIterator
      ->Array.reduceRight(list{}, (acc, (_level, rightmost)) => list{rightmost, ...acc})
      ->List.reverse
    | list{(node, level), ...rest} => {
        let {left, right, val} = node
        levelRightmostTable->Map.set(level, val)

        switch (left, right) {
        | (None, None) => rightSideView(levelRightmostTable, rest)
        | (None, Some(rightNode)) =>
          rightSideView(levelRightmostTable, list{(rightNode, level + 1), ...rest})
        | (Some(leftNode), None) =>
          rightSideView(levelRightmostTable, list{(leftNode, level + 1), ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          rightSideView(
            levelRightmostTable,
            list{(leftNode, level + 1), (rightNode, level + 1), ...rest},
          )
        }
      }
    }
  }

  switch root {
  | None => [] // Empty tree, return empty list

  | Some(node) =>
    let leftBoundary = leftSideView(Map.make(), list{(node, 0)})
    let leaves = collectLeaves(list{}, list{node})
    let rightBoundary = rightSideView(Map.make(), list{(node, 0)})

    [leftBoundary, leaves, rightBoundary]
    ->Array.reduce(Set.make(), (acc, curr) => {
      curr->List.forEach(v => acc->Set.add(v))
      acc
    })
    ->Set.values
    ->Array.fromIterator
  }
}

/**
     1
      \
       2
      / \
     3   4
 */
let tree1 = Some(
  TreeNode.make(
    ~val=1,
    ~right=Some(
      TreeNode.make(~val=2, ~left=Some(TreeNode.make(~val=3)), ~right=Some(TreeNode.make(~val=4))),
    ),
  ),
)
let r1 = boundaryOfBinaryTree(tree1)
Console.log2("r1: ", r1) // [1,3,4,2]

//         _______1_______
//        /               \
//       2                 3
//      / \               /
//     4   5             6
//        / \           / \
//       7   8         9  10
let tree2 = Some(
  TreeNode.make(
    ~val=1,
    ~left=Some(
      TreeNode.make(
        ~val=2,
        ~left=Some(TreeNode.make(~val=4)),
        ~right=Some(
          TreeNode.make(
            ~val=5,
            ~left=Some(TreeNode.make(~val=7)),
            ~right=Some(TreeNode.make(~val=8)),
          ),
        ),
      ),
    ),
    ~right=Some(
      TreeNode.make(
        ~val=3,
        ~left=Some(
          TreeNode.make(
            ~val=6,
            ~left=Some(TreeNode.make(~val=9)),
            ~right=Some(TreeNode.make(~val=10)),
          ),
        ),
      ),
    ),
  ),
)
let r2 = boundaryOfBinaryTree(tree2)
Console.log2("r2: ", r2) // [1,2,4,7,8,9,10,6,3]
