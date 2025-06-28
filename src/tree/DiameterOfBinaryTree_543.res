// T(n) = O(n)
// S(n) = O(n)

type nodeKind = Leaf | Branch

let diameterOfBinaryTree = (root: option<TreeNode.t<int>>) => {
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
            // right first to preserve postorder
            list{rightNode, leftNode, ...rest},
          )
        }
      }
    }
  }

  let rec calculateDiameter = (
    rpnStack: list<(int, nodeKind, int)>,
    diameter: int,
    evalStack: list<(int, nodeKind, int)>,
  ) => {
    switch rpnStack {
    | list{} => diameter + 1
    | list{top, ...rest} => {
        let (_val, nodeKind, leavesCount) = top

        switch nodeKind {
        | Leaf => calculateDiameter(rest, diameter, list{(0, nodeKind, 0), ...evalStack})
        | Branch => {
            let (topLeaves, sliced) = evalStack->List.reduceWithIndex((list{}, list{}), (
              acc,
              curr,
              idx,
            ) => {
              let (topLeaves, sliced) = acc
              let (_subtreeHeight, nodeKind, _leafCount) = curr

              switch leavesCount === 0 {
              | true =>
                idx === 0 ? (list{curr, ...topLeaves}, sliced) : (topLeaves, list{curr, ...sliced})
              | false =>
                idx < leavesCount && nodeKind === Leaf
                  ? (list{curr, ...topLeaves}, sliced)
                  : (topLeaves, list{curr, ...sliced})
              }
            })

            let (leftHeight, rightHeight) = switch topLeaves {
            | list{(h1, _, _), (h2, _, _)} => (h1, h2)
            | list{(h1, _, _)} => (h1, 0)
            | _ => (0, 0)
            }

            let currentDiameter =
              (leftHeight === 0 ? 1 : leftHeight) + (rightHeight === 0 ? 1 : rightHeight)
            let newDiameter = max(diameter, currentDiameter)
            let currentSubtreeHeight = max(leftHeight, rightHeight) + 1

            calculateDiameter(rest, newDiameter, list{(currentSubtreeHeight, Leaf, 0), ...sliced})
          }
        }
      }
    }
  }

  switch root {
  | None => -1
  | Some(node) => postorderTraverse(list{}, list{node})->calculateDiameter(-1, list{})
  }
}

/**
        1
       / \
      2   3
     / \
    4   5
 */
let tree1 = Some(
  TreeNode.make(
    ~val=1,
    ~left=Some(
      TreeNode.make(~val=2, ~left=Some(TreeNode.make(~val=4)), ~right=Some(TreeNode.make(~val=5))),
    ),
    ~right=Some(TreeNode.make(~val=3)),
  ),
)
let r1 = diameterOfBinaryTree(tree1)
Console.log2("r1: ", r1) // 3
