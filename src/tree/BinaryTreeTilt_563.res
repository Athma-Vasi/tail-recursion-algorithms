// T(n) = O(n)
// S(n) = O(n)

type nodeKind = Leaf | Branch

let binaryTreeTilt = (root: option<TreeNode.t<int>>) => {
  // Postorder traversal that builds:
  // - rpnStack: list of nodes with their kinds in Reverse Polish Notation (postorder)
  let rec postorderTraverse = (
    rpnStack: list<(int, nodeKind, int)>, // (val, nodeKind, leavesCount )
    workingStack: list<TreeNode.t<int>>,
  ) => {
    switch workingStack {
    // If no more nodes to process, return the stack and table
    | list{} => rpnStack
    | list{top, ...rest} => {
        let {left, right, val} = top

        switch (left, right) {
        // Case 1: No children → it's a leaf
        | (None, None) => postorderTraverse(list{(val, Leaf, 0), ...rpnStack}, rest)

        // Case 2: Only right child → one leaf child
        | (None, Some(rightNode)) =>
          postorderTraverse(list{(val, Branch, 1), ...rpnStack}, list{rightNode, ...rest})

        // Case 3: Only left child → one leaf child
        | (Some(leftNode), None) =>
          postorderTraverse(list{(val, Branch, 1), ...rpnStack}, list{leftNode, ...rest})

        // Case 4: Both children → two leaf children
        | (Some(leftNode), Some(rightNode)) =>
          postorderTraverse(
            list{(val, Branch, 2), ...rpnStack},
            // Right first to preserve postorder
            list{rightNode, leftNode, ...rest},
          )
        }
      }
    }
  }

  let rec findTiltSum = (
    rpnStack: list<(int, nodeKind, int)>,
    tiltSum: int,
    // (sumSoFar, nodeKind, leavesCount)
    evalStack: list<(int, nodeKind, int)>,
  ): int => {
    switch rpnStack {
    | list{} => tiltSum
    | list{(val, nodeKind, leavesCount), ...rest} =>
      switch nodeKind {
      | Leaf =>
        // Leaf contributes only its own value, no tilt
        findTiltSum(rest, tiltSum, list{(val, nodeKind, 0), ...evalStack})

      | Branch => {
          // Get the top `leavesCount` elements from evalStack
          let (topLeaves, sliced) = evalStack->List.reduceWithIndex((list{}, list{}), (
            acc,
            curr,
            idx,
          ) => {
            let (topLeaves, sliced) = acc
            let (_sumSoFar, nodeKind, _leafCount) = curr

            idx < leavesCount && nodeKind === Leaf
              ? (list{curr, ...topLeaves}, sliced)
              : (topLeaves, list{curr, ...sliced})
          })

          // Compute tilt and subtree sum
          let (newTiltSum, newSubtreeSum) = switch topLeaves {
          | list{} => (tiltSum, val) // No children, degenerate case
          | list{(leftSum, _, _)} => // One child case (only left)
            (tiltSum + abs(leftSum), val + leftSum)
          | list{(leftSum, _, _), (rightSum, _, _), ..._rest} => {
              let tilt = abs(leftSum - rightSum)
              let subtreeSum = val + leftSum + rightSum
              (tiltSum + tilt, subtreeSum)
            }
          }

          // Use Leaf here to mark as computed node
          findTiltSum(rest, newTiltSum, list{(newSubtreeSum, Leaf, 0), ...sliced})
        }
      }
    }
  }

  switch root {
  | None => -1
  | Some(node) => postorderTraverse(list{}, list{node})->findTiltSum(0, list{})
  }
}

//     1
//    / \
//   2   3
let tree1 = Some(
  TreeNode.make(~val=1, ~left=Some(TreeNode.make(~val=2)), ~right=Some(TreeNode.make(~val=3))),
)
let r1 = binaryTreeTilt(tree1)
Console.log2("r1: ", r1) // 1

/**
       4
      / \
     2   9
    / \   \
   3   5   7
 */
let tree2 = Some(
  TreeNode.make(
    ~val=4,
    ~left=Some(
      TreeNode.make(~val=2, ~left=Some(TreeNode.make(~val=3)), ~right=Some(TreeNode.make(~val=5))),
    ),
    ~right=Some(TreeNode.make(~val=9, ~right=Some(TreeNode.make(~val=7)))),
  ),
)
let r2 = binaryTreeTilt(tree2)
Console.log2("r2: ", r2) // 15

/**
           21
         /    \
       7       14
     /   \    /   \
    1     1  2     2
   / \
  3   3
 */
let tree3 = Some(
  TreeNode.make(
    ~val=21,
    ~left=Some(
      TreeNode.make(
        ~val=7,
        ~left=Some(
          TreeNode.make(
            ~val=1,
            ~left=Some(TreeNode.make(~val=3)),
            ~right=Some(TreeNode.make(~val=3)),
          ),
        ),
        ~right=Some(TreeNode.make(~val=1)),
      ),
    ),
    ~right=Some(
      TreeNode.make(~val=14, ~left=Some(TreeNode.make(~val=2)), ~right=Some(TreeNode.make(~val=2))),
    ),
  ),
)
let r3 = binaryTreeTilt(tree3)
Console.log2("r3: ", r3) // 9
