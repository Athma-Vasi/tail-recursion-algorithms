// T(n) = O(n)
// S(n) = O(n)

let countCompleteTreeNodes = (root: option<TreeNode.t<int>>) => {
  let rec preorderTraverse = (count: int, stack: list<TreeNode.t<int>>) => {
    switch stack {
    | list{} => count
    | list{popped, ...rest} => {
        let {left, right} = popped

        switch (left, right) {
        | (None, None) => preorderTraverse(count + 1, rest)
        | (None, Some(rightNode)) => preorderTraverse(count + 1, list{rightNode, ...rest})
        | (Some(leftNode), None) => preorderTraverse(count + 1, list{leftNode, ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          preorderTraverse(count + 1, list{leftNode, rightNode, ...rest})
        }
      }
    }
  }

  switch root {
  | None => 0
  | Some(node) => preorderTraverse(0, list{node})
  }
}

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15))
let r1 = countCompleteTreeNodes(Some(root1))
Console.log2("r1: ", r1) // 6
