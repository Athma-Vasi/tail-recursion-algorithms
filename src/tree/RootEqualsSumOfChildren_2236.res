// T(n) = O(1)
// S(n) = O(1)

let rootEqualsSumOfChildren = (root: option<TreeNode.t<int>>) => {
  switch root {
  | None => false
  | Some(node) => {
      let {val, left, right} = node

      switch (left, right) {
      | (None, None) => false
      | (None, Some(_rNode)) => false
      | (Some(_lNode), None) => false
      | (Some(lNode), Some(rNode)) => val === lNode.val + rNode.val
      }
    }
  }
}

let n4 = TreeNode.make(~val=4)
let n6 = TreeNode.make(~val=6)
let root1 = TreeNode.make(~val=10, ~left=Some(n4), ~right=Some(n6))
let r1 = rootEqualsSumOfChildren(Some(root1))
Console.log2("r1: ", r1)
