// T(n) = O(n^2)
// S(n) = O(n)

let binaryTreeLevelOrderTraversal = (root: option<TreeNode.t<int>>) => {
  let rec traverse = (
    levelValuesTable: Map.t<int, list<int>>,
    queue: list<(TreeNode.t<int>, int)>,
  ) => {
    switch queue {
    | list{} => levelValuesTable
    | list{(node, level), ...rest} => {
        let {left, right, val} = node
        let values =
          levelValuesTable->Map.get(level)->Option.mapOr(list{val}, vals => list{val, ...vals})
        levelValuesTable->Map.set(level, values)

        switch (left, right) {
        | (None, None) => traverse(levelValuesTable, rest)
        | (None, Some(rightNode)) =>
          traverse(levelValuesTable, rest->List.concat(list{(rightNode, level + 1)}))
        | (Some(leftNode), None) =>
          traverse(levelValuesTable, rest->List.concat(list{(leftNode, level + 1)}))
        | (Some(leftNode), Some(rightNode)) =>
          traverse(
            levelValuesTable,
            rest->List.concat(list{(leftNode, level + 1), (rightNode, level + 1)}),
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

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = Some(TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15)))
let r1 = binaryTreeLevelOrderTraversal(root1)
Console.log2("r1: ", r1)
