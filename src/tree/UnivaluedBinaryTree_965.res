// T(n) = O(n)
// S(n) = O(n)

let univaluedBinaryTree = (root: option<TreeNode.t<int>>) => {
  let rec preorderTraverse = (isUnivalued: bool, rootVal: int, stack: list<TreeNode.t<int>>) => {
    switch isUnivalued {
    | false => false
    | true =>
      switch stack {
      | list{} => isUnivalued
      | list{popped, ...rest} => {
          let {left, right, val} = popped

          switch (left, right) {
          | (None, None) => preorderTraverse(val === rootVal, rootVal, rest)
          | (None, Some(rightNode)) =>
            preorderTraverse(val === rootVal, rootVal, list{rightNode, ...rest})
          | (Some(leftNode), None) =>
            preorderTraverse(val === rootVal, rootVal, list{leftNode, ...rest})
          | (Some(leftNode), Some(rightNode)) =>
            preorderTraverse(val === rootVal, rootVal, list{leftNode, rightNode, ...rest})
          }
        }
      }
    }
  }

  switch root {
  | None => false
  | Some(node) => preorderTraverse(true, node.val, list{node})
  }
}

let tree: option<TreeNode.t<int>> = Some(
  TreeNode.make(
    ~val=1,
    ~left=Some(TreeNode.make(~val=1, ~left=Some(TreeNode.make(~val=1)))),
    ~right=Some(TreeNode.make(~val=1, ~right=Some(TreeNode.make(~val=1)))),
  ),
)
let r1 = univaluedBinaryTree(tree)
Console.log2("r1: ", r1) // true

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15))
let r2 = univaluedBinaryTree(Some(root1))
Console.log2("r2: ", r2) // false
