// T(n) = O(n)
// S(n) = O(h)

let sameTree = (root1: option<TreeNode.t<int>>, root2: option<TreeNode.t<int>>) => {
  // inorder
  let rec traverse = (
    areSame: bool,
    curr1: option<TreeNode.t<int>>,
    curr2: option<TreeNode.t<int>>,
    stack1: list<TreeNode.t<int>>,
    stack2: list<TreeNode.t<int>>,
  ) => {
    // Early exit if a mismatch has already occurred
    switch areSame {
    | false => false
    | true =>
      switch (curr1, curr2) {
      // If both current nodes are None, pop the next nodes from stacks to compare their values
      | (None, None) =>
        switch (stack1, stack2) {
        // Both stacks are empty — traversal complete and all nodes matched
        | (list{}, list{}) => areSame
        // One stack is empty, the other isn't — tree structures differ
        | (list{}, list{_popped2, ..._rest2}) => false
        | (list{_popped1, ..._rest1}, list{}) => false
        // Both stacks have nodes — compare popped node values, then traverse right subnode
        | (list{popped1, ...rest1}, list{popped2, ...rest2}) =>
          // Move to right subtree of both nodes. Continue with remaining stack
          traverse(popped1.val === popped2.val, popped1.right, popped2.right, rest1, rest2)
        }
      // One tree has a node, the other does not — structure mismatch
      | (None, Some(_node2)) => false
      | (Some(_node1), None) => false
      // Both current nodes exist — move down the left subtree, pushing current nodes onto the stack
      | (Some(node1), Some(node2)) =>
        // Traverse left subtrees, adding curr nodes to stacks
        traverse(areSame, node1.left, node2.left, list{node1, ...stack1}, list{node2, ...stack2})
      }
    }
  }

  traverse(true, root1, root2, list{}, list{})
}

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15))

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root2 = TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15))

let r1 = sameTree(Some(root1), Some(root2))
Console.log2("r1: ", r1) // true

let tree1 = Some(
  TreeNode.make(
    ~val=1,
    ~left=Some(TreeNode.make(~val=2)),
    ~right=Some(TreeNode.make(~val=3, ~left=Some(TreeNode.make(~val=4)))),
  ),
)
let tree2 = Some(
  TreeNode.make(~val=1, ~left=Some(TreeNode.make(~val=2)), ~right=Some(TreeNode.make(~val=4))),
)
let r2 = sameTree(tree1, tree2)
Console.log2("r2: ", r2)
