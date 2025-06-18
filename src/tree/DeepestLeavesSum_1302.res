// T(n) = O(n)
// S(n) = O(n)

let deepestLeavesSum = (root: option<TreeNode.t<int>>) => {
  let rec reversePostorderTraverse = (
    maxLevel: int,
    levelValueStack: list<(int, int)>, // (value, level)
    workingStack: list<(TreeNode.t<int>, int)>,
  ) => {
    switch workingStack {
    | list{} => (maxLevel, levelValueStack)
    | list{(top, level), ...rest} => {
        let {left, right, val} = top
        let newLVStack = list{(val, level), ...levelValueStack}
        let newMaxLevel = maxLevel > level ? maxLevel : level

        switch (left, right) {
        | (None, None) => reversePostorderTraverse(newMaxLevel, newLVStack, rest)
        | (None, Some(rightNode)) =>
          reversePostorderTraverse(newMaxLevel, newLVStack, list{(rightNode, level + 1), ...rest})
        | (Some(leftNode), None) =>
          reversePostorderTraverse(newMaxLevel, newLVStack, list{(leftNode, level + 1), ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          reversePostorderTraverse(
            newMaxLevel,
            newLVStack,
            list{(leftNode, level + 1), (rightNode, level + 1), ...rest},
          )
        }
      }
    }
  }

  switch root {
  | None => 0
  | Some(node) => {
      let (maxLevel, levelValueStack) = reversePostorderTraverse(0, list{}, list{(node, 1)})
      levelValueStack->List.reduce(0, (acc, curr) => {
        let (val, level) = curr
        level === maxLevel ? acc + val : acc
      })
    }
  }
}

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = Some(TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15)))
let r1 = deepestLeavesSum(root1)
Console.log2("r1: ", r1) // 28
