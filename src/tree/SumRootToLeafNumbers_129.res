// T(n) = O(n)
// S(n) = O(n)

let sumRootToLeafNumbers = (root: option<TreeNode.t<int>>) => {
  let rec preorderTraverse = (sum: int, stack: list<(TreeNode.t<int>, string)>) => {
    switch stack {
    | list{} => sum
    | list{(node, collected), ...rest} => {
        let {left, right, val} = node
        let newCollected = collected ++ Int.toString(val)

        switch (left, right) {
        | (None, None) =>
          preorderTraverse(sum + Int.fromString(newCollected)->Option.mapOr(0, s => s), rest)
        | (None, Some(rightNode)) => preorderTraverse(sum, list{(rightNode, newCollected), ...rest})
        | (Some(leftNode), None) => preorderTraverse(sum, list{(leftNode, newCollected), ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          preorderTraverse(sum, list{(leftNode, newCollected), (rightNode, newCollected), ...rest})
        }
      }
    }
  }

  switch root {
  | None => 0
  | Some(node) => preorderTraverse(0, list{(node, "")})
  }
}

/**
        10
       /  \
      5    15
     / \     \
    3   7     18
 */
let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = Some(TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15)))
let r1 = sumRootToLeafNumbers(root1)
Console.log2("r1: ", r1) // 103628
