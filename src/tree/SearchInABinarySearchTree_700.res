// T(n) = O(n)
// S(n) = O(n)

let searchInABinarySearchTree = (root: option<TreeNode.t<int>>, val: int) => {
  let rec inorderTraverse = (
    result: option<TreeNode.t<int>>,
    curr: option<TreeNode.t<int>>,
    stack: list<TreeNode.t<int>>,
  ): option<TreeNode.t<int>> => {
    switch curr {
    | None =>
      switch stack {
      | list{} => result
      | list{popped, ...rest} =>
        inorderTraverse(popped.val === val ? Some(popped) : result, popped.right, rest)
      }
    | Some(node) => inorderTraverse(result, node.left, list{node, ...stack})
    }
  }

  inorderTraverse(None, root, list{})
}

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = Some(TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15)))
let r1 = searchInABinarySearchTree(root1, 7)
Console.log2("r1: ", r1)
// {
//   val: 7,
//   left: undefined,
//   right: undefined,
// }

let r2 = searchInABinarySearchTree(root1, 11)
Console.log2("r2: ", r2) // undefined
