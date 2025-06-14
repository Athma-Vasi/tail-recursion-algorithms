// T(n) = O(n)
// S(n) = O(n)

let binaryTreePaths = (root: option<TreeNode.t<int>>) => {
  let rec traverse = (paths: list<string>, stack: list<(TreeNode.t<int>, string)>) => {
    switch stack {
    | list{} => paths
    | list{(poppedNode, path), ...rest} => {
        let {left, right} = poppedNode

        switch (left, right) {
        // Leaf node — add path to result, continue with rest of stack
        | (None, None) => traverse(list{path, ...paths}, rest)
        | (None, Some(rightNode)) => {
            let rightPath = path->String.concat("->" ++ rightNode.val->Int.toString)
            traverse(paths, list{(rightNode, rightPath), ...rest})
          }
        | (Some(leftNode), None) => {
            let leftPath = path->String.concat("->" ++ leftNode.val->Int.toString)
            traverse(paths, list{(leftNode, leftPath), ...rest})
          }
        | (Some(leftNode), Some(rightNode)) => {
            let leftPath = path->String.concat("->" ++ leftNode.val->Int.toString)
            let rightPath = path->String.concat("->" ++ rightNode.val->Int.toString)

            traverse(paths, list{(leftNode, leftPath), (rightNode, rightPath), ...rest})
          }
        }
      }
    }
  }

  switch root {
  | None => []
  | Some(node) => traverse(list{}, list{(node, node.val->Int.toString)})->List.toArray
  }
}

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15))
let r1 = binaryTreePaths(Some(root1))
Console.log2("r1: ", r1)
