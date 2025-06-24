// T(n) = O(n * log(n))
// S(n) = O(n)

let findLevelOfTreeWithMinimumSum = (root: option<TreeNode.t<int>>) => {
  let rec preorderTraverse = (
    levelSumsTable: Map.t<int, int>,
    stack: list<(TreeNode.t<int>, int)>,
  ) => {
    switch stack {
    | list{} => levelSumsTable
    | list{(node, level), ...rest} => {
        let {left, right, val} = node
        let sums = levelSumsTable->Map.get(level)->Option.getOr(0)
        levelSumsTable->Map.set(level, sums + val)

        switch (left, right) {
        | (None, None) => preorderTraverse(levelSumsTable, rest)
        | (None, Some(rightNode)) =>
          preorderTraverse(levelSumsTable, list{(rightNode, level + 1), ...rest})
        | (Some(leftNode), None) =>
          preorderTraverse(levelSumsTable, list{(leftNode, level + 1), ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          preorderTraverse(
            levelSumsTable,
            list{(leftNode, level + 1), (rightNode, level + 1), ...rest},
          )
        }
      }
    }
  }

  switch root {
  | None => -1
  | Some(node) =>
    let (level, _minSum) =
      preorderTraverse(Map.make(), list{(node, 1)})
      ->Map.entries
      ->Array.fromIterator
      ->Array.toSorted(((level1, sum1), (level2, sum2)) =>
        sum1 === sum2 ? Int.compare(level1, level2) : Int.compare(sum1, sum2)
      )
      ->Array.at(0)
      ->Option.mapOr((-1, 0), tuple => tuple)

    level
  }
}

/**
       50
       /  \
      6    2
     / \   /
   30  80 7
 */
let tree1 = Some(
  TreeNode.make(
    ~val=50,
    ~left=Some(
      TreeNode.make(
        ~val=6,
        ~left=Some(TreeNode.make(~val=30)),
        ~right=Some(TreeNode.make(~val=80)),
      ),
    ),
    ~right=Some(TreeNode.make(~val=2, ~left=Some(TreeNode.make(~val=7)))),
  ),
)
let r1 = findLevelOfTreeWithMinimumSum(tree1)
Console.log2("r1: ", r1) // 2

/**
       36
      /  \
    17    10
          /
        24
 */
let tree2 = Some(
  TreeNode.make(
    ~val=36,
    ~left=Some(TreeNode.make(~val=17)), // 17 has no children (both are None by default)
    ~right=Some(TreeNode.make(~val=10, ~left=Some(TreeNode.make(~val=24)))), // 24 is the left child of 10
  ),
)
let r2 = findLevelOfTreeWithMinimumSum(tree2)
Console.log2("r2: ", r2) // 3

/**
    5
     \
      5
        \
         5
 */
let tree3 = Some(
  TreeNode.make(~val=5, ~right=Some(TreeNode.make(~val=5, ~right=Some(TreeNode.make(~val=5))))),
)
let r3 = findLevelOfTreeWithMinimumSum(tree3)
Console.log2("r3: ", r3) // 1
