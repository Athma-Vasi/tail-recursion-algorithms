// T(n) = O(n)
// S(n) = O(n)

let naryTreeLevelOrderTraversal = (root: option<NaryTreeNode.t<int>>) => {
  let rec traverse = (
    levelValuesTable: Map.t<int, list<int>>,
    queue: list<(NaryTreeNode.t<int>, int)>,
  ) => {
    switch queue {
    | list{} => levelValuesTable
    | list{(node, level), ...rest} => {
        let {branches, val} = node
        let values =
          levelValuesTable->Map.get(level)->Option.mapOr(list{val}, vals => list{val, ...vals})
        levelValuesTable->Map.set(level, values)

        switch branches {
        | None => traverse(levelValuesTable, rest)
        | Some(branches) =>
          traverse(
            levelValuesTable,
            branches
            ->List.reverse
            ->List.reduce(rest, (acc, branch) => {
              list{(branch, level + 1), ...acc}
            }),
          )
        }
      }
    }
  }

  switch root {
  | None => []
  | Some(node) =>
    traverse(Map.make(), list{(node, 0)})
    ->Map.values
    ->Array.fromIterator
    ->Array.reduce(list{}, (acc, values) => {
      list{values->List.reverse->List.toArray, ...acc}
    })
    ->List.reverse
    ->List.toArray
  }
}

/**
            1
         /  |  \
       2    3    4
     / | \   |   | \
    5  6  7  8   9 10
 */
let // Leaf nodes (depth 3)
n5 = NaryTreeNode.make(~val=5)
let n6 = NaryTreeNode.make(~val=6)
let n7 = NaryTreeNode.make(~val=7)
let n8 = NaryTreeNode.make(~val=8)
let n9 = NaryTreeNode.make(~val=9)
let n10 = NaryTreeNode.make(~val=10)

// Depth 2 nodes with children
let n2 = NaryTreeNode.make(~val=2, ~branches=Some(list{n5, n6, n7}))
let n3 = NaryTreeNode.make(~val=3, ~branches=Some(list{n8}))
let n4 = NaryTreeNode.make(~val=4, ~branches=Some(list{n9, n10}))

// Root node with children at depth 2
let root2 = Some(NaryTreeNode.make(~val=1, ~branches=Some(list{n2, n3, n4})))
let r2 = naryTreeLevelOrderTraversal(root2)
Console.log2("r2: ", r2) // [ [ 1 ], [ 2, 3, 4 ], [ 5, 6, 7, 8, 9, 10 ] ]
