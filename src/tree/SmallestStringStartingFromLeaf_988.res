// T(n) = O(n * h * log n)
// S(n) =O(n * h)

let smallestStringStartingFromLeaf = (root: option<TreeNode.t<int>>) => {
  let rec preorderTraverse = (paths: list<string>, stack: list<(TreeNode.t<int>, string)>) => {
    switch stack {
    | list{} => paths
    | list{(node, pathSoFar), ...rest} => {
        let {left, right, val} = node
        let char = String.fromCharCode(97 + val)
        let newPath = char ++ pathSoFar

        switch (left, right) {
        | (None, None) => preorderTraverse(list{newPath, ...paths}, rest)
        | (None, Some(rightNode)) => preorderTraverse(paths, list{(rightNode, newPath), ...rest})
        | (Some(leftNode), None) => preorderTraverse(paths, list{(leftNode, newPath), ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          preorderTraverse(paths, list{(leftNode, newPath), (rightNode, newPath), ...rest})
        }
      }
    }
  }

  switch root {
  | None => String.make()
  | Some(node) =>
    preorderTraverse(list{}, list{(node, String.make())})
    ->List.sort((s1, s2) => String.compare(s1, s2))
    ->List.head
    ->Option.mapOr(String.make(), str => str)
  }
}

/**
        0
      /   \
     1     2
    / \   / \
   3   4 3   4
 */
let tree1 = Some(
  TreeNode.make(
    ~val=0,
    ~left=Some(
      TreeNode.make(~val=1, ~left=Some(TreeNode.make(~val=3)), ~right=Some(TreeNode.make(~val=4))),
    ),
    ~right=Some(
      TreeNode.make(~val=2, ~left=Some(TreeNode.make(~val=3)), ~right=Some(TreeNode.make(~val=4))),
    ),
  ),
)
let r1 = smallestStringStartingFromLeaf(tree1)
Console.log2("r1: ", r1) // "dba"

/**
         25
       /    \
      1      3
     / \    / \
    1   3  0   2
 */
let tree2 = Some(
  TreeNode.make(
    ~val=25,
    ~left=Some(
      TreeNode.make(~val=1, ~left=Some(TreeNode.make(~val=1)), ~right=Some(TreeNode.make(~val=3))),
    ),
    ~right=Some(
      TreeNode.make(~val=3, ~left=Some(TreeNode.make(~val=0)), ~right=Some(TreeNode.make(~val=2))),
    ),
  ),
)
let r2 = smallestStringStartingFromLeaf(tree2)
Console.log2("r2: ", r2) // "adz"

/**
            2
          /   \
         2     1
          \    /
           1  0
          /
         0
 */
let tree3 = Some(
  TreeNode.make(
    ~val=2,
    ~left=Some(
      TreeNode.make(~val=2, ~right=Some(TreeNode.make(~val=1, ~left=Some(TreeNode.make(~val=0))))),
    ),
    ~right=Some(TreeNode.make(~val=1, ~left=Some(TreeNode.make(~val=0)))),
  ),
)
let r3 = smallestStringStartingFromLeaf(tree3)
Console.log2("r3: ", r3) // "abc"
