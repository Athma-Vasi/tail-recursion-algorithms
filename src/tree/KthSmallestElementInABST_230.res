// T(n) = O(n)
// S(n) = O(n)

let kthSmallestElementInABST = (root: option<TreeNode.t<int>>, k: int) => {
  let rec inorderTraverse = (
    sortedAscStack: list<int>,
    curr: option<TreeNode.t<int>>,
    workingStack: list<TreeNode.t<int>>,
  ) => {
    switch curr {
    | None =>
      switch workingStack {
      | list{} => sortedAscStack
      | list{top, ...rest} => inorderTraverse(list{top.val, ...sortedAscStack}, top.right, rest)
      }
    | Some(node) => inorderTraverse(sortedAscStack, node.left, list{node, ...workingStack})
    }
  }

  inorderTraverse(list{}, root, list{})
  ->List.reverse
  ->List.get(k - 1) // k is 1-indexed
  ->Option.mapOr(-1, num => num)
}

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = Some(TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15)))
let r1 = kthSmallestElementInABST(root1, 1)
Console.log2("r1: ", r1) // 3
