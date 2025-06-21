// T(n) = O(n)
// S(n) = O(n)

let binaryTreeLevelOrderTraversal_II = (root: option<TreeNode.t<int>>) => {
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
    ->Array.reduce(list{}, (acc, values) => {
      list{values->List.reverse->List.toArray, ...acc}
    })
    ->List.toArray
  }
}

/**
         3
       / \
      9  20
         / \
        15  7
 */
let tree1 = Some(
  TreeNode.make(
    ~val=3,
    ~left=Some(TreeNode.make(~val=9)),
    ~right=Some(
      TreeNode.make(
        ~val=20,
        ~left=Some(TreeNode.make(~val=15)),
        ~right=Some(TreeNode.make(~val=7)),
      ),
    ),
  ),
)
let r1 = binaryTreeLevelOrderTraversal_II(tree1)
Console.log2("r1: ", r1) // [[15,7],[9,20],[3]]
