// T(n) = O(n^2)
// S(n) = O(n)

let minNumberOfOpsToSortBinaryTreeByLevel = (root: option<TreeNode.t<int>>) => {
  // Helper function to perform BFS and collect node values level by level
  let rec breadthFirstTraverse = (
    // <level, values>: keeps track of all node values at each depth level
    levelValuesTable: Map.t<int, list<int>>,
    // (node, level): queue of nodes to process with their associated levels
    queue: list<(TreeNode.t<int>, int)>,
  ) => {
    switch queue {
    | list{} => levelValuesTable // base case: return table when queue is empty
    | list{(node, level), ...rest} => {
        let {left, right, val} = node
        // Get existing values at this level and append current node's value
        let values =
          levelValuesTable
          ->Map.get(level)
          // Values currently in right-to-left order for efficiency
          ->Option.mapOr(list{val}, vals => list{val, ...vals})
        // Mutates map to update values for the current level
        levelValuesTable->Map.set(level, values)

        // Add children to the queue with incremented level
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

  // Helper function to calculate minimum number of swaps needed to sort an array
  let getMinimumSwaps = (values: array<int>) => {
    // Recursive implementation of swap counting using tracking and mutation
    let rec minSwaps = (
      swaps: int,
      valuesPositionsTable: Map.t<int, int>, // stores current positions of values
      cloned: array<int>, // mutable working copy of the array
      sorted: array<int>, // target sorted array
      // current index to process
      index: int,
    ) => {
      switch index === Array.length(cloned) {
      | true => swaps // base case: all elements checked
      | false => {
          // Get current and target value at index
          let originalValue = cloned->Array.at(index)->Option.mapOr(-1, v => v)
          let sortedValue = sorted->Array.at(index)->Option.mapOr(-1, v => v)

          switch originalValue === sortedValue {
          | true => minSwaps(swaps, valuesPositionsTable, cloned, sorted, index + 1)
          | false => {
              // Value mismatch: need a swap
              // Find where the correct value currently is
              let currPosition =
                valuesPositionsTable->Map.get(sortedValue)->Option.mapOr(-1, p => p)
              // Update the position map for the swapped value
              valuesPositionsTable->Map.set(originalValue, currPosition)

              minSwaps(
                swaps + 1,
                valuesPositionsTable,
                // Swap in the cloned array
                cloned->Array.mapWithIndex((val, idx) =>
                  idx === currPosition ? originalValue : val
                ),
                sorted,
                index + 1,
              )
            }
          }
        }
      }
    }

    // Initialize map of value → index, and start recursion
    minSwaps(
      0,
      values->Array.reduceWithIndex(Map.make(), (acc, curr, idx) => {
        acc->Map.set(curr, idx)
        acc
      }),
      values->Array.map(v => v),
      values->Array.toSorted((v1, v2) => Int.compare(v1, v2)),
      0,
    )
  }

  // Entry point
  switch root {
  | None => 0 // empty tree: no swaps needed
  | Some(node) =>
    // Traverse and build level → node values mapping
    breadthFirstTraverse(Map.make(), list{(node, 0)})
    ->Map.values // get values per level
    ->Array.fromIterator
    // Calculate swaps for each level and sum
    ->Array.reduce(0, (acc, curr) => {
      // Reverse values to left-to-right before calculating swaps
      acc + getMinimumSwaps(curr->List.reverse->List.toArray)
    })
  }
}

/**
            1
          /   \
         4     3
        / \   / \
       7  6  8   5
                  \
                   9
                    \
                    10
 */
let node10 = TreeNode.make(~val=10)
let node9 = TreeNode.make(~val=9, ~right=Some(node10))

// Layer above
let node5 = TreeNode.make(~val=5, ~right=Some(node9))
let node8 = TreeNode.make(~val=8)
let node6 = TreeNode.make(~val=6)
let node7 = TreeNode.make(~val=7)

// Next layer
let node4 = TreeNode.make(~val=4, ~left=Some(node7), ~right=Some(node6))
let node3 = TreeNode.make(~val=3, ~left=Some(node8), ~right=Some(node5))

// Root
let root2 = Some(TreeNode.make(~val=1, ~left=Some(node4), ~right=Some(node3)))
let r2 = minNumberOfOpsToSortBinaryTreeByLevel(root2)
Console.log2("r2: ", r2) // 3
