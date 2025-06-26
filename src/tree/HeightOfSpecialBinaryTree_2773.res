// T(n) = O(n)
// S(n) = O(n)

let heightOfSpecialBinaryTree = (root: option<TreeNode.t<int>>) => {
  //   let rec preorderTraverse = (
  //     height: int,
  //     // (node, parentNode, heightSoFar)
  //     stack: list<(TreeNode.t<int>, TreeNode.t<int>, int)>,
  //   ) => {
  //     switch stack {
  //     | list{} => height
  //     | list{(node, parentNode, heightSoFar), ...rest} => {
  //         let {left, right, val} = node
  //         let newHeight = heightSoFar > height ? heightSoFar : height

  //         switch (left, right) {
  //         | (None, None) => preorderTraverse(newHeight, rest)
  //         // root.right.left !== root
  //         | (None, Some(rightNode)) =>
  //           switch rightNode.left {
  //           | None => preorderTraverse(newHeight, list{(rightNode, node, heightSoFar + 1), ...rest})
  //           | Some(lNode) =>
  //             lNode === parentNode
  //               ? preorderTraverse(newHeight, list{(rightNode, node, heightSoFar), ...rest})
  //               : preorderTraverse(newHeight, list{(rightNode, node, heightSoFar + 1), ...rest})
  //           }
  //         // root.left.right !== root
  //         | (Some(leftNode), None) =>
  //           switch leftNode.right {
  //           | None => preorderTraverse(newHeight, list{(leftNode, node, heightSoFar + 1), ...rest})
  //           | Some(rNode) =>
  //             rNode === parentNode
  //               ? preorderTraverse(newHeight, list{(leftNode, node, heightSoFar), ...rest})
  //               : preorderTraverse(newHeight, list{(leftNode, node, heightSoFar + 1), ...rest})
  //           }
  //         | (Some(leftNode), Some(rightNode)) =>
  //           switch (rightNode.left, leftNode.right) {
  //           | (None, None) =>
  //             preorderTraverse(
  //               newHeight,
  //               list{(leftNode, node, heightSoFar + 1), (rightNode, node, heightSoFar + 1), ...rest},
  //             )
  //           | (None, Some(rNode)) =>
  //             rNode === parentNode
  //               ? preorderTraverse(newHeight, list{(leftNode, node, heightSoFar), ...rest})
  //               : preorderTraverse(newHeight, list{(leftNode, node, heightSoFar + 1), ...rest})
  //           | (Some(lNode), None) =>
  //             lNode === parentNode
  //               ? preorderTraverse(newHeight, list{(rightNode, node, heightSoFar), ...rest})
  //               : preorderTraverse(newHeight, list{(rightNode, node, heightSoFar + 1), ...rest})
  //           | (Some(lNode), Some(rNode)) =>
  //             lNode === parentNode || rNode === parentNode
  //               ? preorderTraverse(
  //                   newHeight,
  //                   list{(leftNode, node, heightSoFar), (rightNode, node, heightSoFar), ...rest},
  //                 )
  //               : preorderTraverse(
  //                   newHeight,
  //                   list{
  //                     (leftNode, node, heightSoFar + 1),
  //                     (rightNode, node, heightSoFar + 1),
  //                     ...rest,
  //                   },
  //                 )
  //           }
  //         }
  //       }
  //     }
  //   }

  let rec preorderTraverse = (
    height: int,
    // (node, parent, heightSoFar)
    stack: list<(TreeNode.t<int>, TreeNode.t<int>, int)>,
  ) => {
    switch stack {
    | list{} => height
    | list{(node, parentNode, heightSoFar), ...rest} => {
        let {left, right} = node
        let newHeight = max(height, heightSoFar)

        switch (left, right) {
        | (None, None) => preorderTraverse(newHeight, rest)

        | (None, Some(rightNode)) =>
          switch rightNode.left {
          | Some(l) if l === parentNode => preorderTraverse(newHeight, rest)
          | _ => preorderTraverse(newHeight, list{(rightNode, node, heightSoFar + 1), ...rest})
          }

        | (Some(leftNode), None) =>
          switch leftNode.right {
          | Some(r) if r === parentNode => preorderTraverse(newHeight, rest)
          | _ => preorderTraverse(newHeight, list{(leftNode, node, heightSoFar + 1), ...rest})
          }

        | (Some(leftNode), Some(rightNode)) =>
          let leftBack = switch leftNode.right {
          | Some(r) => r === parentNode
          | None => false
          }

          let rightBack = switch rightNode.left {
          | Some(l) => l === parentNode
          | None => false
          }

          let next = switch (leftBack, rightBack) {
          | (true, true) => rest
          | (true, false) => list{(rightNode, node, heightSoFar + 1), ...rest}
          | (false, true) => list{(leftNode, node, heightSoFar + 1), ...rest}
          | (false, false) =>
            list{(leftNode, node, heightSoFar + 1), (rightNode, node, heightSoFar + 1), ...rest}
          }

          preorderTraverse(newHeight, next)
        }
      }
    }
  }

  switch root {
  | None => 0
  | Some(node) => preorderTraverse(0, list{(node, node, 0)})
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
    ~left=Some(TreeNode.make(~val=2)),
    ~right=Some(
      TreeNode.make(~val=3, ~left=Some(TreeNode.make(~val=4)), ~right=Some(TreeNode.make(~val=5))),
    ),
  ),
)
let r1 = heightOfSpecialBinaryTree(tree1)
Console.log2("r1: ", r1) // 2
