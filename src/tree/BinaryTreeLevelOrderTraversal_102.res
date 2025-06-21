// T(n) = O(n)
// S(n) = O(n)
let binaryTreeLevelOrderTraversal = (root: option<TreeNode.t<int>>) => {
  let rec breadthFirstTraverse = (
    levelValuesTable: Map.t<int, list<int>>,
    queue: array<(TreeNode.t<int>, int)>,
  ) => {
    switch queue->Array.at(0) {
    | None => levelValuesTable
    | Some((node, level)) => {
        let rest = queue->Array.sliceToEnd(~start=1)
        let {left, right, val} = node
        let values =
          levelValuesTable->Map.get(level)->Option.mapOr(list{val}, vals => list{val, ...vals})
        levelValuesTable->Map.set(level, values)

        switch (left, right) {
        | (None, None) => breadthFirstTraverse(levelValuesTable, rest)
        | (None, Some(rightNode)) => {
            rest->Array.push((rightNode, level + 1))
            breadthFirstTraverse(levelValuesTable, rest)
          }
        | (Some(leftNode), None) => {
            rest->Array.push((leftNode, level + 1))
            breadthFirstTraverse(levelValuesTable, rest)
          }
        | (Some(leftNode), Some(rightNode)) => {
            rest->Array.push((leftNode, level + 1))
            rest->Array.push((rightNode, level + 1))
            breadthFirstTraverse(levelValuesTable, rest)
          }
        }
      }
    }
  }

  switch root {
  | None => []
  | Some(node) =>
    breadthFirstTraverse(Map.make(), list{(node, 0)})
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
