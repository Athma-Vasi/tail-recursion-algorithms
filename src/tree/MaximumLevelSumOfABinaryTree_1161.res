// T(n) = O(n)
// S(n) = O(n)

let maximumLevelSumOfABinaryTree = (root: option<TreeNode.t<int>>) => {
  let rec traverse = (
    levelValuesTable: Map.t<int, list<int>>,
    stack: list<(TreeNode.t<int>, int)>,
  ) => {
    switch stack {
    | list{} => levelValuesTable
    | list{(node, level), ...rest} => {
        let {left, right, val} = node
        let values =
          levelValuesTable->Map.get(level)->Option.mapOr(list{val}, vals => list{val, ...vals})
        levelValuesTable->Map.set(level, values)

        switch (left, right) {
        | (None, None) => traverse(levelValuesTable, rest)
        | (None, Some(rightNode)) =>
          traverse(levelValuesTable, list{(rightNode, level + 1), ...rest})
        | (Some(leftNode), None) => traverse(levelValuesTable, list{(leftNode, level + 1), ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          traverse(levelValuesTable, list{(leftNode, level + 1), (rightNode, level + 1), ...rest})
        }
      }
    }
  }

  switch root {
  | None => -1
  | Some(node) =>
    traverse(Map.make(), list{(node, 1)})
    ->Map.entries
    ->Array.fromIterator
    ->Array.reduce(list{}, (acc, (level, values)) => {
      let sum = values->List.reduce(0, (sumAcc, num) => sumAcc + num)
      list{(level, sum), ...acc}
    })
    ->List.toArray
    ->Array.toSorted((tuple1, tuple2) => {
      let (level1, sum1) = tuple1
      let (level2, sum2) = tuple2
      sum1 === sum2 ? Int.compare(level1, level2) : Int.compare(sum2, sum1)
    })
    ->Array.reduceWithIndex(-1, (acc, (level, _sum), idx) => {
      idx === 0 ? level : acc
    })
  }
}

/**
        1
       / \
      7   0
     / \
    7  -8
 */
let tree1: option<TreeNode.t<int>> = Some(
  TreeNode.make(
    ~val=1,
    ~left=Some(
      TreeNode.make(~val=7, ~left=Some(TreeNode.make(~val=7)), ~right=Some(TreeNode.make(~val=-8))),
    ),
    ~right=Some(TreeNode.make(~val=0)),
  ),
)
let r1 = maximumLevelSumOfABinaryTree(tree1)
Console.log2("r1: ", r1) // 2
