// T(n) = O(n)
// S(n) = O(n)

let findLargestValueInEachTreeRow = (root: option<TreeNode.t<int>>) => {
  let rec preorderTraverse = (
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
        | (None, None) => preorderTraverse(levelValuesTable, rest)
        | (None, Some(rightNode)) =>
          preorderTraverse(levelValuesTable, list{(rightNode, level + 1), ...rest})
        | (Some(leftNode), None) =>
          preorderTraverse(levelValuesTable, list{(leftNode, level + 1), ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          preorderTraverse(
            levelValuesTable,
            list{(leftNode, level + 1), (rightNode, level + 1), ...rest},
          )
        }
      }
    }
  }

  switch root {
  | None => []
  | Some(node) =>
    preorderTraverse(Map.make(), list{(node, 0)})
    ->Map.values
    ->Array.fromIterator
    ->Array.reduceRight(list{}, (acc, values) => {
      list{
        values->List.reduce(Int32.min_int, (largest, value) => largest > value ? largest : value),
        ...acc,
      }
    })
    ->List.toArray
  }
}

/**
        1
      /   \
     3     2
    / \     \
   5   3     9
 */
let tree1 = Some(
  TreeNode.make(
    ~val=1,
    ~left=Some(
      TreeNode.make(~val=3, ~left=Some(TreeNode.make(~val=5)), ~right=Some(TreeNode.make(~val=3))),
    ),
    ~right=Some(TreeNode.make(~val=2, ~right=Some(TreeNode.make(~val=9)))),
  ),
)
let r1 = findLargestValueInEachTreeRow(tree1)
Console.log2("r1: ", r1) // [1, 3, 9]
