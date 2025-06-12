// T(n) = O(n)
// S(n) = O(n)

let rangeSumOfBST = (root: TreeNode.t<int>, low: int, high: int) => {
  // inorder
  let rec traverse = (result: int, curr: option<TreeNode.t<int>>, stack: list<TreeNode.t<int>>) => {
    switch curr {
    | None =>
      switch stack {
      | list{} => result
      | list{top, ...rest} =>
        let {val, right} = top
        traverse(val >= low && val <= high ? result + val : result, right, rest)
      }
    | Some(node) => traverse(result, node.left, list{node, ...stack})
    }
  }

  traverse(0, Some(root), list{})
}

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15))
let r1 = rangeSumOfBST(root1, 7, 15)
Console.log2("r1: ", r1) // 32

let n5 = TreeNode.make(~val=5)
let n17 = TreeNode.make(~val=17)
let n15 = TreeNode.make(~val=15, ~right=Some(n17))
let n10 = TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15))
let n40 = TreeNode.make(~val=40)
let n30 = TreeNode.make(~val=30, ~right=Some(n40))
let root2 = TreeNode.make(~val=20, ~left=Some(n10), ~right=Some(n30))
let r2 = rangeSumOfBST(root2, 10, 35)
Console.log2("r2: ", r2) // 92
