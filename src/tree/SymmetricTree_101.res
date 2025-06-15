let symmetricTree = (root: option<TreeNode.t<int>>) => {
  let rec breadthFirstTraverse = (
    levelValuesTable: Map.t<int, list<int>>,
    stack: list<(TreeNode.t<int>, int)>,
  ) => {
    switch stack {
    // Base case: traversal complete, return the populated map
    | list{} => levelValuesTable
    | list{(poppedNode, level), ...rest} => {
        let {left, right, val} = poppedNode
        // Get the current values at this level, and prepend the current node value.
        let values =
          levelValuesTable
          ->Map.get(level)
          ->Option.mapOr(list{val}, vals => list{val, ...vals})
        // Update the map with the new list of values for this level
        levelValuesTable->Map.set(level, values)

        // Continue traversal by adding children (if any) to the stack at the next level
        switch (left, right) {
        | (None, None) => breadthFirstTraverse(levelValuesTable, rest)
        | (None, Some(rightNode)) =>
          breadthFirstTraverse(levelValuesTable, list{(rightNode, level + 1), ...rest})
        | (Some(leftNode), None) =>
          breadthFirstTraverse(levelValuesTable, list{(leftNode, level + 1), ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          breadthFirstTraverse(
            levelValuesTable,
            list{(leftNode, level + 1), (rightNode, level + 1), ...rest},
          )
        }
      }
    }
  }

  // Check whether a list of integers is a palindrome
  let checkIsPalindrome = (items: list<int>) => {
    // Recursive helper that checks equality of outermost elements
    let rec check = (checks: Set.t<bool>, sliced: array<int>) => {
      let length = Array.length(sliced)

      switch length === 0 {
      // If the set contains `false`, then not palindromic
      | true => !Set.has(checks, false)
      | false => {
          // Reduce to extract first and last elements, build new array without them
          let ((first, last), newSliced) = sliced->Array.reduceWithIndex(
            ((Int32.min_int, Int32.max_int), []),
            (acc, num, idx) => {
              let ((first, last), newSliced) = acc
              idx === 0
                ? ((num, last), newSliced)
                : idx === length - 1
                ? ((first, num), newSliced)
                // Middle values preserved
                : ((first, last), newSliced->Array.concat([num]))
            },
          )
          checks->Set.add(first === last)
          // Continue recursively with reduced array
          check(checks, newSliced)
        }
      }
    }

    check(Set.make(), List.toArray(items))
  }

  switch root {
  // An empty tree is symmetric
  | None => true
  | Some(node) => {
      let checks =
        // Build the level-wise map via BFS
        breadthFirstTraverse(Map.make(), list{(node, 0)})
        ->Map.entries
        ->Core__Iterator.toArray
        // Reduce across all levels to collect palindrome checks in a Set<bool>
        ->Array.reduce(Set.make(), (acc, (level, values)) => {
          // Root level is always symmetric
          acc->Set.add(level === 0 ? true : checkIsPalindrome(values))
          acc
        })

      // If the set contains `false`, then at least one level was not palindromic
      !Set.has(checks, false)
    }
  }
}

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = Some(TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15)))
let r1 = symmetricTree(root1)
Console.log2("r1: ", r1) // false

let symmetric = Some(
  TreeNode.make(
    ~val=1,
    ~left=Some(
      TreeNode.make(~val=2, ~left=Some(TreeNode.make(~val=3)), ~right=Some(TreeNode.make(~val=4))),
    ),
    ~right=Some(
      TreeNode.make(~val=2, ~left=Some(TreeNode.make(~val=4)), ~right=Some(TreeNode.make(~val=3))),
    ),
  ),
)
let r2 = symmetricTree(symmetric)
Console.log2("r2: ", r2) // true
