// T(n) = O(n)
// S(n) = O(n)

let binaryTreeInorderTraversal = (root: option<TreeNode.t<int>>) => {
  let rec traverse = (
    resultStack: list<int>,
    curr: option<TreeNode.t<int>>,
    workingStack: list<TreeNode.t<int>>,
  ) => {
    switch curr {
    | None =>
      switch workingStack {
      | list{} => resultStack
      | list{popped, ...rest} => traverse(list{popped.val, ...resultStack}, popped.right, rest)
      }
    | Some(node) => traverse(resultStack, node.left, list{node, ...workingStack})
    }
  }

  traverse(list{}, root, list{})->List.reverse->List.toArray
}

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15))
let r1 = binaryTreeInorderTraversal(Some(root1))
Console.log2("r1: ", r1)
