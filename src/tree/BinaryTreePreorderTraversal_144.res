// T(n) = O(n)
// S(n) = O(n)

let binaryTreePreorderTraversal = (root: option<TreeNode.t<int>>) => {
  let rec traverse = (result: list<int>, stack: list<TreeNode.t<int>>) => {
    switch stack {
    | list{} => result
    | list{popped, ...rest} => {
        let {left, right, val} = popped
        let newResult = list{val, ...result}

        switch (left, right) {
        | (None, None) => traverse(newResult, rest)
        | (None, Some(rightNode)) => traverse(newResult, list{rightNode, ...rest})
        | (Some(leftNode), None) => traverse(newResult, list{leftNode, ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          traverse(newResult, list{leftNode, rightNode, ...rest})
        }
      }
    }
  }

  switch root {
  | None => []
  | Some(node) => traverse(list{}, list{node})->List.reverse->List.toArray
  }
}

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15))
let r1 = binaryTreePreorderTraversal(Some(root1))
Console.log2("r1: ", r1) // [ 10, 5, 3, 7, 15, 18 ]
