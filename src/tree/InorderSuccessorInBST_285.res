// T(n) = O(n)
// S(n) = O(n)

type state = FoundVal | NotFound | FoundSuccessor

let inorderSuccessorInBST = (root: option<TreeNode.t<int>>, p: int) => {
  let rec inorderTraverse = (
    successor: option<int>,
    curr: option<TreeNode.t<int>>,
    state,
    stack: list<TreeNode.t<int>>,
  ) => {
    switch curr {
    | None =>
      switch (state, stack) {
      | (NotFound, list{}) => successor
      | (NotFound, list{node, ...rest}) => inorderTraverse(successor, node.right, state, rest)
      | (FoundSuccessor, list{}) => successor
      | (FoundSuccessor, list{_node, ..._rest}) => successor
      | (FoundVal, list{}) => successor
      | (FoundVal, list{node, ..._rest}) =>
        inorderTraverse(Some(node.val), None, FoundSuccessor, list{})
      }
    | Some(node) =>
      p === node.val
        ? inorderTraverse(successor, None, FoundVal, stack)
        : inorderTraverse(successor, node.left, state, list{node, ...stack})
    }
  }

  inorderTraverse(None, root, NotFound, list{})
}

/**
    2
   / \
  1   3
 */
let tree1 = Some(
  TreeNode.make(~val=2, ~left=Some(TreeNode.make(~val=1)), ~right=Some(TreeNode.make(~val=3))),
)
let r1 = inorderSuccessorInBST(tree1, 1)
Console.log2("r1:", r1) // 2

/**
        5
       / \
      3   6
     / \
    2   4
   /
  1
 */
let tree2 = Some(
  TreeNode.make(
    ~val=5,
    ~left=Some(
      TreeNode.make(
        ~val=3,
        ~left=Some(TreeNode.make(~val=2, ~left=Some(TreeNode.make(~val=1)))),
        ~right=Some(TreeNode.make(~val=4)),
      ),
    ),
    ~right=Some(TreeNode.make(~val=6)),
  ),
)
let r2 = inorderSuccessorInBST(tree2, 6)
Console.log2("r2:", r2) // undefined (None)
