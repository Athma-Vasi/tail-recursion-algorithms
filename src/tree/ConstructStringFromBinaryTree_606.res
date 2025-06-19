// T(n) = O(n)
// S(n) = O(n)
// INCORRECT: cannot figure out how to track sibling node in preorder traversal
// for correct number of closing parentheses

let constructStringFromBinaryTree = (root: option<TreeNode.t<int>>) => {
  let rec preorderTraverse = (result: string, stack: list<(TreeNode.t<int>, int)>) => {
    switch stack {
    | list{} => result
    | list{(node, level), ...rest} => {
        let {left, right, val} = node

        switch (left, right) {
        | (None, None) =>
          preorderTraverse(
            level === 0
              ? result ++ Int.toString(val)
              : result ++
                "(" ++
                Int.toString(val) ++
                Array.make(~length=level, "")->Array.reduce(String.make(), (acc, _curr) => {
                  acc ++ ")"
                }),
            rest,
          )
        | (None, Some(rightNode)) =>
          preorderTraverse(
            level === 0
              ? result ++ Int.toString(val) ++ "(()("
              : result ++ "(" ++ Int.toString(val) ++ "()",
            list{(rightNode, level + 1), ...rest},
          )
        | (Some(leftNode), None) =>
          preorderTraverse(
            level === 0
              ? result ++ Int.toString(val) ++ "(("
              : result ++ "(" ++ Int.toString(val) ++ "(",
            list{(leftNode, level + 1), ...rest},
          )
        | (Some(leftNode), Some(rightNode)) =>
          preorderTraverse(
            level === 0 ? result ++ Int.toString(val) : result ++ "(" ++ Int.toString(val),
            list{(leftNode, level + 1), (rightNode, level + 1), ...rest},
          )
        }
      }
    }
  }

  switch root {
  | None => String.make()
  | Some(node) => preorderTraverse(String.make(), list{(node, 0)})
  }
}

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = Some(TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15)))
let r1 = constructStringFromBinaryTree(root1)
Console.log2("r1: ", r1)
