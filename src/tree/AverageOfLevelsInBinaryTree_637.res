// T(n) = O(n)
// S(n) = O(n)

let averageOfLevelsInBinaryTree = (root: option<TreeNode.t<int>>) => {
  // Recursive function to perform BFS and collect values at each level
  // `levelsValuesTable`: map from level -> list of node values at that level
  // `stack`: list of (node, level) pairs to process
  let rec breadthFirstTraverse = (
    levelsValuesTable: Map.t<int, list<int>>,
    stack: list<(TreeNode.t<int>, int)>,
  ) => {
    switch stack {
    // If the stack is empty, return the accumulated map
    | list{} => levelsValuesTable

    // Process the head of the stack
    | list{(poppedNode, level), ...rest} => {
        let {left, right, val} = poppedNode

        // Get existing values at this level or start a new list
        let values =
          levelsValuesTable
          ->Map.get(level)
          ->Option.mapOr(list{val}, vals => list{val, ...vals})
        levelsValuesTable->Map.set(level, values)

        // Add child nodes to the stack with incremented level
        switch (left, right) {
        | (None, None) => breadthFirstTraverse(levelsValuesTable, rest)
        | (None, Some(rightNode)) =>
          breadthFirstTraverse(levelsValuesTable, list{(rightNode, level + 1), ...rest})
        | (Some(leftNode), None) =>
          breadthFirstTraverse(levelsValuesTable, list{(leftNode, level + 1), ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          breadthFirstTraverse(
            levelsValuesTable,
            list{(leftNode, level + 1), (rightNode, level + 1), ...rest},
          )
        }
      }
    }
  }

  // if root is None, return empty array; otherwise start BFS from root at level 0
  switch root {
  | None => []
  | Some(node) =>
    breadthFirstTraverse(Map.make(), list{(node, 0)})
    ->Map.values // Get all lists of values for each level
    ->Core__Iterator.toArray // Convert iterator to array
    ->Array.reduce(list{}, (averages, values) => {
      // Sum all values at the level
      let sum = values->List.reduce(0, (sumAcc, num) => sumAcc + num)
      let size = values->List.size
      let avg = sum / size
      list{avg, ...averages} // Prepend to averages list in reverse order
    })
    ->List.reverse
    ->List.toArray
  }
}

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15))
let r1 = averageOfLevelsInBinaryTree(Some(root1))
Console.log2("r1: ", r1) // [9, 10, 10]
