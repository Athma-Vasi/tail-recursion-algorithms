// T(n) = O(n log n)
// S(n) = O(n)

let closestBinarySearchTreeValue = (root: option<TreeNode.t<int>>, target: float) => {
  let rec inorderTraverse = (
    valuesDistances: list<(int, float)>,
    curr: option<TreeNode.t<int>>,
    stack: list<TreeNode.t<int>>,
  ) => {
    switch curr {
    | None =>
      switch stack {
      | list{} => valuesDistances
      | list{node, ...rest} => {
          let {right, val} = node
          let distance = Int.toFloat(val) -. target
          let absDistance = distance < 1.0 ? distance *. -1.0 : distance

          inorderTraverse(list{(val, absDistance), ...valuesDistances}, right, rest)
        }
      }
    | Some(node) => inorderTraverse(valuesDistances, node.left, list{node, ...stack})
    }
  }

  inorderTraverse(list{}, root, list{})
  ->List.sort(((_value1, distance1), (_value2, distance2)) => {
    Float.compare(distance1, distance2)
  })
  ->List.head
  ->Option.mapOr(-1, ((value, _distance)) => value)
}

/**
       4
     /   \
    2     5
   / \
  1   3
 */
let tree1 = Some(
  TreeNode.make(
    ~val=4,
    ~left=Some(
      TreeNode.make(~val=2, ~left=Some(TreeNode.make(~val=1)), ~right=Some(TreeNode.make(~val=3))),
    ),
    ~right=Some(TreeNode.make(~val=5)),
  ),
)
let r1 = closestBinarySearchTreeValue(tree1, 3.714286)
Console.log2("r1: ", r1) // 4
