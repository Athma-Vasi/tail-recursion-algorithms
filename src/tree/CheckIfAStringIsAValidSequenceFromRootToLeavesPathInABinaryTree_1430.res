// T(n) = O(n)
// S(n) = O(n)

let checkIfAStringIsAValidSequenceFromRootToLeavesPathInABinaryTree = (
  root: option<TreeNode.t<int>>,
  sequence: string,
) => {
  let rec preorderTraverse = (
    paths: list<list<int>>,
    stack: list<(TreeNode.t<int>, list<int>)>,
  ) => {
    switch stack {
    | list{} => paths
    | list{(node, pathSoFar), ...rest} => {
        let {left, right, val} = node
        let newPathSoFar = list{val, ...pathSoFar}

        switch (left, right) {
        | (None, None) => preorderTraverse(list{newPathSoFar, ...paths}, rest)
        | (None, Some(rightNode)) =>
          preorderTraverse(paths, list{(rightNode, newPathSoFar), ...rest})
        | (Some(leftNode), None) => preorderTraverse(paths, list{(leftNode, newPathSoFar), ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          preorderTraverse(
            paths,
            list{(leftNode, newPathSoFar), (rightNode, newPathSoFar), ...rest},
          )
        }
      }
    }
  }

  switch root {
  | None => false
  | Some(node) =>
    preorderTraverse(list{}, list{(node, list{})})->List.reduce(false, (acc, path) => {
      let serialized =
        path
        ->List.reverse
        ->List.reduce(String.make(), (seqAcc, pos) => seqAcc ++ Int.toString(pos))

      serialized === sequence || acc
    })
  }
}

/**
         0
       / \
      1   0
     / \  /
    0  1 0
    \  /\ 
    1 0  0
*/
let tree1 = Some(
  TreeNode.make(
    ~val=0,
    ~left=Some(
      TreeNode.make(
        ~val=1,
        ~left=Some(TreeNode.make(~val=0, ~right=Some(TreeNode.make(~val=1)))),
        ~right=Some(
          TreeNode.make(
            ~val=1,
            ~left=Some(
              TreeNode.make(
                ~val=1,
                ~left=Some(TreeNode.make(~val=0)),
                ~right=Some(TreeNode.make(~val=0)),
              ),
            ),
          ),
        ),
      ),
    ),
    ~right=Some(TreeNode.make(~val=0, ~left=Some(TreeNode.make(~val=0)))),
  ),
)
let r1 = checkIfAStringIsAValidSequenceFromRootToLeavesPathInABinaryTree(tree1, "0101")
Console.log2("r1: ", r1) // true

let r2 = checkIfAStringIsAValidSequenceFromRootToLeavesPathInABinaryTree(tree1, "001")
Console.log2("r2: ", r2) // false

let r3 = checkIfAStringIsAValidSequenceFromRootToLeavesPathInABinaryTree(tree1, "011")
Console.log2("r3: ", r3) // false
