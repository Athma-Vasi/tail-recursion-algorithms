// T(n, m) = O(n + m)
// S(n, m) = O(h1 + h2)

let leafSimilarTrees = (root1: option<TreeNode.t<int>>, root2: option<TreeNode.t<int>>) => {
  // Recursive function to perform synchronized inorder traversal of both trees
  let rec inorderTraverse = (
    leafSimilar: bool,
    curr1: option<TreeNode.t<int>>,
    curr2: option<TreeNode.t<int>>,
    stack1: list<TreeNode.t<int>>,
    stack2: list<TreeNode.t<int>>,
  ) => {
    switch leafSimilar {
    // if already determined not leaf-similar, return early
    | false => false
    | true =>
      switch (curr1, curr2) {
      // if both current nodes are None, proceed to pop from stacks
      | (None, None) =>
        switch (stack1, stack2) {
        // if both stacks are empty, we're done
        | (list{}, list{}) => leafSimilar

        // if only one stack is empty, mismatch in number of leaves
        | (list{}, list{_node2, ..._rest2}) => false
        | (list{_node1, ..._rest1}, list{}) => false

        // pop a node from each stack and check leaf conditions
        | (list{popped1, ...rest1}, list{popped2, ...rest2}) =>
          switch (popped1.left, popped1.right, popped2.left, popped2.right) {
          // if both nodes are leaves, compare their values
          | (None, None, None, None) =>
            inorderTraverse(popped1.val === popped2.val, None, None, rest1, rest2)

          // if not both are leaves, continue rightward traversal
          | _ => inorderTraverse(leafSimilar, popped1.right, popped2.right, rest1, rest2)
          }
        }

      // mismatch if only one current node is present
      | (None, Some(_node2)) => false
      | (Some(_node1), None) => false

      // if both current nodes exist, descend to their left children
      | (Some(node1), Some(node2)) =>
        inorderTraverse(
          leafSimilar,
          node1.left,
          node2.left,
          list{node1, ...stack1},
          list{node2, ...stack2},
        )
      }
    }
  }

  inorderTraverse(true, root1, root2, list{}, list{})
}

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15))
let r1 = leafSimilarTrees(Some(root1), Some(root1))
Console.log2("r1: ", r1) // true

let tree: option<TreeNode.t<int>> = Some(
  TreeNode.make(
    ~val=1,
    ~left=Some(TreeNode.make(~val=1, ~left=Some(TreeNode.make(~val=1)))),
    ~right=Some(TreeNode.make(~val=1, ~right=Some(TreeNode.make(~val=1)))),
  ),
)
let r2 = leafSimilarTrees(Some(root1), tree)
Console.log2("r2: ", r2) // false
