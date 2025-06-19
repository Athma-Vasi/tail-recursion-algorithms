// T(n) = O(n)
// S(n) = O(n)

let findBottomLeftTreeValue = (root: option<TreeNode.t<int>>) => {
  // Perform a breadth-first traversal of the tree while keeping track of level-wise values.
  let rec breadthFirstTraverse = (
    levelValuesTable: Map.t<int, list<int>>, // Stores values for each level
    // Holds nodes along with their corresponding level
    queue: list<(TreeNode.t<int>, int)>,
  ) => {
    switch queue {
    | list{} => levelValuesTable // When the queue is empty, traversal is complete

    | list{(node, level), ...rest} => {
        let {left, right, val} = node

        // Retrieve existing values for the level or initialize with current node's value
        let values =
          levelValuesTable
          ->Map.get(level)
          ->Option.mapOr(list{val}, vals => list{val, ...vals}) // Prepend current value

        // Update the map for this level with the new values list
        levelValuesTable->Map.set(level, values)

        // Enqueue child nodes with incremented level
        switch (left, right) {
        | (None, None) => breadthFirstTraverse(levelValuesTable, rest)
        | (None, Some(rightNode)) =>
          breadthFirstTraverse(levelValuesTable, rest->List.concat(list{(rightNode, level + 1)}))
        | (Some(leftNode), None) =>
          breadthFirstTraverse(levelValuesTable, rest->List.concat(list{(leftNode, level + 1)}))
        | (Some(leftNode), Some(rightNode)) =>
          breadthFirstTraverse(
            levelValuesTable,
            rest->List.concat(list{(leftNode, level + 1), (rightNode, level + 1)}),
          )
        }
      }
    }
  }

  switch root {
  | None => 0 // If tree is empty, return 0
  | Some(node) =>
    // Traverse the tree and collect level-wise values
    breadthFirstTraverse(Map.make(), list{(node, 0)})
    ->Map.values // Extract the grouped values for all levels
    ->Array.fromIterator // Convert iterator to array
    ->Array.at(-1) // Get the values of the deepest level (last in array)
    ->Option.mapOr(list{}, values => values) // If exists, use the list of values
    ->List.reverse // Reverse to access leftmost value (which was added last)
    ->List.head // Get the first value, i.e., the leftmost at the deepest level
    ->Option.mapOr(0, v => v) // If exists, return it; otherwise return 0
  }
}

/**
        1
       / \
      2   3
     /   / \
    4   5   6
          /
        7
 */
let node7 = TreeNode.make(~val=7)
let node5 = TreeNode.make(~val=5, ~left=Some(node7))
let node6 = TreeNode.make(~val=6)
let node4 = TreeNode.make(~val=4)
let node2 = TreeNode.make(~val=2, ~left=Some(node4))
let node3 = TreeNode.make(~val=3, ~left=Some(node5), ~right=Some(node6))
let root = Some(TreeNode.make(~val=1, ~left=Some(node2), ~right=Some(node3)))
let r1 = findBottomLeftTreeValue(root)
Console.log2("r1: ", r1) // 7
