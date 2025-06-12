// T(n) = O(n)
// S(n) = O(n)

let findACorrespondingNodeOfABinaryTreeInACloneOfThatTree = (
  original: TreeNode.t<int>,
  cloned: TreeNode.t<int>,
  target: TreeNode.t<int>,
): option<TreeNode.t<int>> => {
  // inorder
  let rec traverse = (
    currOriginal: option<TreeNode.t<int>>,
    currCloned: option<TreeNode.t<int>>,
    originalsStack: list<TreeNode.t<int>>,
    clonedsStack: list<TreeNode.t<int>>,
  ) => {
    switch (currOriginal, currCloned) {
    // need to pop from stacks
    | (None, None) =>
      switch (originalsStack, clonedsStack) {
      // fully traversed the tree without finding target
      | (list{}, list{}) => None
      // malformed state
      | (list{}, list{_topCloned, ..._restCloneds}) => None
      // malformed state
      | (list{_topOriginal, ..._restOriginals}, list{}) => None
      // pop the top node from each stack to continue right subtree traversal
      | (list{topOriginal, ...restOriginals}, list{topCloned, ...restCloneds}) =>
        switch target.val === topOriginal.val {
        | true => Some(topCloned) // success: return cloned node
        // continue traversal to right child nodes
        | false => traverse(topOriginal.right, topCloned.right, restOriginals, restCloneds)
        }
      }
    // tree structure mismatch
    | (None, Some(_clonedNode)) => None
    // tree structure mismatch
    | (Some(_originalNode), None) => None
    // move down the left subtree, pushing nodes onto stacks
    | (Some(originalNode), Some(clonedNode)) =>
      traverse(
        originalNode.left,
        clonedNode.left,
        list{originalNode, ...originalsStack},
        list{clonedNode, ...clonedsStack},
      )
    }
  }

  traverse(Some(original), Some(cloned), list{}, list{})
}

let o6 = TreeNode.make(~val=6)
let o19 = TreeNode.make(~val=19)
let o3 = TreeNode.make(~val=3, ~left=Some(o6), ~right=Some(o19))
let o4 = TreeNode.make(~val=4)
let oRoot1 = TreeNode.make(~val=7, ~left=Some(o4), ~right=Some(o3))

let c6 = TreeNode.make(~val=6)
let c19 = TreeNode.make(~val=19)
let c3 = TreeNode.make(~val=3, ~left=Some(c6), ~right=Some(c19))
let c4 = TreeNode.make(~val=4)
let cRoot1 = TreeNode.make(~val=7, ~left=Some(c4), ~right=Some(c3))
let r1 = findACorrespondingNodeOfABinaryTreeInACloneOfThatTree(oRoot1, cRoot1, o3)
Console.log2("r1: ", r1)
