// T(n) = O(n)
// S(n) = O(n)
// INCORRECT
// fails when p and q are not in same level

type nodeKind = Leaf | Branch

let lowestCommonAncestorOfABinaryTree_III = (root: option<TreeNode.t<int>>, p: int, q: int) => {}

/**
        3
       / \
      5   1
     / \  / \
    6  2 0  8
      / \
     7   4
 */
let tree1 = Some(
  TreeNode.make(
    ~val=3,
    ~left=Some(
      TreeNode.make(
        ~val=5,
        ~left=Some(TreeNode.make(~val=6)),
        ~right=Some(
          TreeNode.make(
            ~val=2,
            ~left=Some(TreeNode.make(~val=7)),
            ~right=Some(TreeNode.make(~val=4)),
          ),
        ),
      ),
    ),
    ~right=Some(
      TreeNode.make(~val=1, ~left=Some(TreeNode.make(~val=0)), ~right=Some(TreeNode.make(~val=8))),
    ),
  ),
)
let r1 = lowestCommonAncestorOfABinaryTree_III(tree1, 5, 1)
Console.log2("r1: ", r1) // 3

/**
 [
  [ 6, "Leaf" ], [ 7, "Leaf" ], [ 4, "Leaf" ], [ 2, "Branch" ], [ 5, "Branch" ],
  [ 0, "Leaf" ], [ 8, "Leaf" ], [ 1, "Branch" ], [ 3, "Branch" ]
]
 */
/**
        3    
       / \
      5   1
     / \ / \
    6  2 0  8
      / \
     7   4
 */
let tree2 = Some(
  TreeNode.make(
    ~val=3,
    ~left=Some(
      TreeNode.make(
        ~val=5,
        ~left=Some(TreeNode.make(~val=6)),
        ~right=Some(
          TreeNode.make(
            ~val=2,
            ~left=Some(TreeNode.make(~val=7)),
            ~right=Some(TreeNode.make(~val=4)),
          ),
        ),
      ),
    ),
    ~right=Some(
      TreeNode.make(~val=1, ~left=Some(TreeNode.make(~val=0)), ~right=Some(TreeNode.make(~val=8))),
    ),
  ),
)
let r2 = lowestCommonAncestorOfABinaryTree_III(tree2, 5, 4)
Console.log2("r2: ", r2) // 5
