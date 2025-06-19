// T(n) = O(n)
// S(n) = O(n)

let countGoodNodesInBinaryTree = (root: option<TreeNode.t<int>>) => {
  let rec preorderTraverse = (count: int, stack: list<(TreeNode.t<int>, int)>) => {
    switch stack {
    | list{} => count
    | list{(node, maxSoFar), ...rest} => {
        let {left, right, val} = node
        let newMax = val >= maxSoFar ? val : maxSoFar
        let newCount = val >= maxSoFar ? count + 1 : count

        switch (left, right) {
        | (None, None) => preorderTraverse(newCount, rest)
        | (None, Some(rightNode)) => preorderTraverse(newCount, list{(rightNode, newMax), ...rest})
        | (Some(leftNode), None) => preorderTraverse(newCount, list{(leftNode, newMax), ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          preorderTraverse(newCount, list{(leftNode, newMax), (rightNode, newMax), ...rest})
        }
      }
    }
  }

  switch root {
  | None => 0
  | Some(node) => preorderTraverse(0, list{(node, node.val - 1)})
  }
}

/**
        3
       / \
      1   4
     /   / \
    3   1   5
 */
let node3_left = TreeNode.make(~val=3)
let node1_right = TreeNode.make(~val=1)
let node5 = TreeNode.make(~val=5)

let node1 = TreeNode.make(~val=1, ~left=Some(node3_left))
let node4 = TreeNode.make(~val=4, ~left=Some(node1_right), ~right=Some(node5))

let root = Some(TreeNode.make(~val=3, ~left=Some(node1), ~right=Some(node4)))
let r1 = countGoodNodesInBinaryTree(root)
Console.log2("r1: ", r1) // 4
