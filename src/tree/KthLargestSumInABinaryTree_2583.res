// T(n) = O(n)
// S(n) = O(n)

let kthLargestSumInABinaryTree = (root: option<TreeNode.t<int>>, k: int) => {
  let rec preorderTraverse = (
    levelValuesTable: Map.t<int, list<int>>,
    maxLevel: int,
    stack: list<(TreeNode.t<int>, int)>,
  ) => {
    switch stack {
    | list{} => (levelValuesTable, maxLevel)
    | list{(node, level), ...rest} => {
        let {left, right, val} = node
        let values =
          levelValuesTable->Map.get(level)->Option.mapOr(list{val}, vals => list{val, ...vals})
        levelValuesTable->Map.set(level, values)
        let newMaxLevel = maxLevel > level ? maxLevel : level

        switch (left, right) {
        | (None, None) => preorderTraverse(levelValuesTable, newMaxLevel, rest)
        | (None, Some(rightNode)) =>
          preorderTraverse(levelValuesTable, newMaxLevel, list{(rightNode, level + 1), ...rest})
        | (Some(leftNode), None) =>
          preorderTraverse(levelValuesTable, newMaxLevel, list{(leftNode, level + 1), ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          preorderTraverse(
            levelValuesTable,
            newMaxLevel,
            list{(leftNode, level + 1), (rightNode, level + 1), ...rest},
          )
        }
      }
    }
  }

  switch root {
  | None => -1
  | Some(node) => {
      let (levelValuesTable, maxLevel) = preorderTraverse(Map.make(), -1, list{(node, 0)})

      switch maxLevel < k - 1 {
      | true => -1
      | false =>
        levelValuesTable
        ->Map.values
        ->Array.fromIterator
        ->Array.reduce(list{}, (acc, values) => {
          list{values->List.reduce(0, (sum, val) => sum + val), ...acc}
        })
        ->List.toArray
        ->Array.toSorted((v1, v2) => Int.compare(v2, v1))
        ->Array.at(k - 1)
        ->Option.mapOr(-1, n => n)
      }
    }
  }
}

/**
        5
       / \
      8   9
     / \ / \
    2  1 3  7
   / \
  4   6
 */
let tree1 = Some(
  TreeNode.make(
    ~val=5,
    ~left=Some(
      TreeNode.make(
        ~val=8,
        ~left=Some(
          TreeNode.make(
            ~val=2,
            ~left=Some(TreeNode.make(~val=4)),
            ~right=Some(TreeNode.make(~val=6)),
          ),
        ),
        ~right=Some(TreeNode.make(~val=1)),
      ),
    ),
    ~right=Some(
      TreeNode.make(~val=9, ~left=Some(TreeNode.make(~val=3)), ~right=Some(TreeNode.make(~val=7))),
    ),
  ),
)
let r1 = kthLargestSumInABinaryTree(tree1, 2)
Console.log2("r1: ", r1) // 13

/**
    1
   /
  2
 /
3
 */
let tree2 = Some(
  TreeNode.make(~val=1, ~left=Some(TreeNode.make(~val=2, ~left=Some(TreeNode.make(~val=3))))),
)
let r2 = kthLargestSumInABinaryTree(tree2, 1)
Console.log2("r2: ", r2) // 3
