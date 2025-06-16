// T(n) = O(n)
// S(n) = O(n)

let binaryTreePostorderTraversal = (root: option<TreeNode.t<int>>) => {
  let rec traverse = (resultStack: list<int>, workingStack: list<TreeNode.t<int>>) => {
    switch workingStack {
    // Base case: all nodes processed, return result
    | list{} => resultStack
    | list{popped, ...rest} => {
        let {left, right, val} = popped
        // Item is pushed to stack then processed
        let newResultStack = list{val, ...resultStack}

        switch (left, right) {
        | (None, None) => traverse(newResultStack, rest)
        | (None, Some(rightNode)) => traverse(newResultStack, list{rightNode, ...rest})
        | (Some(leftNode), None) => traverse(newResultStack, list{leftNode, ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          // Preorder traversal that is root->right->left
          traverse(newResultStack, list{rightNode, leftNode, ...rest})
        }
      }
    }
  }

  switch root {
  | None => []
  | Some(node) => traverse(list{}, list{node})->List.toArray
  }
}

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = Some(TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15)))
let r1 = binaryTreePostorderTraversal(root1)
Console.log2("r1: ", r1) // [ 3, 7, 5, 18, 15, 10 ]
