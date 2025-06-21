// T(n) = O(n)
// S(n) = O(n)

let binaryTreeRightSideView = (root: option<TreeNode.t<int>>) => {
  let rec preorderTraverse = (
    levelRightmostTable: Map.t<int, int>,
    stack: list<(TreeNode.t<int>, int)>,
  ) => {
    switch stack {
    | list{} => levelRightmostTable
    | list{(node, level), ...rest} => {
        let {left, right, val} = node
        levelRightmostTable->Map.set(level, val)

        switch (left, right) {
        | (None, None) => preorderTraverse(levelRightmostTable, rest)
        | (None, Some(rightNode)) =>
          preorderTraverse(levelRightmostTable, list{(rightNode, level + 1), ...rest})
        | (Some(leftNode), None) =>
          preorderTraverse(levelRightmostTable, list{(leftNode, level + 1), ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          preorderTraverse(
            levelRightmostTable,
            list{(leftNode, level + 1), (rightNode, level + 1), ...rest},
          )
        }
      }
    }
  }

  switch root {
  | None => []
  | Some(node) =>
    preorderTraverse(Map.make(), list{(node, 1)})
    ->Map.entries
    ->Array.fromIterator
    ->Array.reduceRight(list{}, (acc, (_level, rightmost)) => list{rightmost, ...acc})
    ->List.toArray
  }
}

/**
         1
        / \
       2   3
      /       
     4         
    / 
   5
 */
let tree1: option<TreeNode.t<int>> = Some(
  TreeNode.make(
    ~val=1,
    ~left=Some(
      TreeNode.make(~val=2, ~left=Some(TreeNode.make(~val=4, ~left=Some(TreeNode.make(~val=5))))),
    ),
    ~right=Some(TreeNode.make(~val=3)),
  ),
)
let r1 = binaryTreeRightSideView(tree1)
Console.log2("r1: ", r1) // [1,3,4,5]

/**
        1
       / \
      2   3
       \     \
        5     4
 */
let tree2: option<TreeNode.t<int>> = Some(
  TreeNode.make(
    ~val=1,
    ~left=Some(TreeNode.make(~val=2, ~right=Some(TreeNode.make(~val=5)))),
    ~right=Some(TreeNode.make(~val=3, ~right=Some(TreeNode.make(~val=4)))),
  ),
)
let r2 = binaryTreeRightSideView(tree2)
Console.log2("r2: ", r2) // [1,3,4]
