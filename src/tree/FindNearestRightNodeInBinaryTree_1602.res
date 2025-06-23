// T(n) = O(n)
// S(n) = O(n)

let findNearestRightNodeInBinaryTree = (root: option<TreeNode.t<int>>, u: int) => {
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
  | None => Int32.min_int
  | Some(node) =>
    preorderTraverse(Map.make(), list{(node, 0)})
    ->Map.values
    ->Array.fromIterator
    ->Array.reduce(Int32.min_int, (acc, values) => {
      let uIdx = values->List.reduceWithIndex(-1, (idxAcc, value, idx) => {
        switch value === u {
        // values are in reverse order: right-to-left
        | true => values->List.get(idx - 1)->Option.mapOr(Int32.min_int, v => v)
        | false => idxAcc
        }
      })
      uIdx >= 0 ? uIdx : acc
    })
  }
}

/**
        1
       / \
      2   3
       \  / \
        4 5 6
 */
let tree1 = Some(
  TreeNode.make(
    ~val=1,
    ~left=Some(TreeNode.make(~val=2, ~left=None, ~right=Some(TreeNode.make(~val=4)))),
    ~right=Some(
      TreeNode.make(~val=3, ~left=Some(TreeNode.make(~val=5)), ~right=Some(TreeNode.make(~val=6))),
    ),
  ),
)
let r1 = findNearestRightNodeInBinaryTree(tree1, 4)
Console.log2("r1: ", r1) // 5 
