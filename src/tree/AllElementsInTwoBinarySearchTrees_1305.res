// T(m, n) = O(max(m, n))
// S(m, n) = O(max(m, n))

let allElementsInTwoBinarySearchTrees = (
  root1: option<TreeNode.t<int>>,
  root2: option<TreeNode.t<int>>,
) => {
  let rec inorderTraverse = (
    resultStack: list<int>,
    curr1: option<TreeNode.t<int>>,
    curr2: option<TreeNode.t<int>>,
    stack1: list<TreeNode.t<int>>,
    stack2: list<TreeNode.t<int>>,
  ) => {
    switch (curr1, curr2) {
    | (None, None) =>
      switch (stack1, stack2) {
      | (list{}, list{}) => resultStack
      | (list{}, list{top2, ...rest2}) =>
        inorderTraverse(list{top2.val, ...resultStack}, curr1, top2.right, stack1, rest2)
      | (list{top1, ...rest1}, list{}) =>
        inorderTraverse(list{top1.val, ...resultStack}, top1.right, curr2, rest1, stack2)
      | (list{top1, ...rest1}, list{top2, ...rest2}) =>
        inorderTraverse(
          top1.val < top2.val
            ? list{top1.val, top2.val, ...resultStack}
            : list{top2.val, top1.val, ...resultStack},
          top1.right,
          top2.right,
          rest1,
          rest2,
        )
      }
    | (None, Some(node2)) =>
      inorderTraverse(resultStack, curr1, node2.left, stack1, list{node2, ...stack2})
    | (Some(node1), None) =>
      inorderTraverse(resultStack, node1.left, curr2, list{node1, ...stack1}, stack2)
    | (Some(node1), Some(node2)) =>
      inorderTraverse(
        resultStack,
        node1.left,
        node2.left,
        list{node1, ...stack1},
        list{node2, ...stack2},
      )
    }
  }

  inorderTraverse(list{}, root1, root2, list{}, list{})->List.reverse->List.toArray
}

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = Some(TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15)))
let r1 = allElementsInTwoBinarySearchTrees(root1, Some(n5))
Console.log2("r1: ", r1) // list{ 3, 3, 5, 5, 7, 7, 10, 15, 18 }
