// T(n) = O(n)
// S(n) = O(n)

let pathSum_II = (root: option<TreeNode.t<int>>, targetSum: int) => {
  let rec preorderTraverse = (
    paths: list<list<int>>,
    stack: list<(TreeNode.t<int>, list<int>)>,
  ) => {
    switch stack {
    | list{} => paths
    | list{(node, pathSoFar), ...rest} => {
        let {left, right, val} = node
        let newPath = list{val, ...pathSoFar}

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
  | None => []
  | Some(node) =>
    preorderTraverse(list{}, list{(node, list{})})
    ->List.reduce(list{}, (acc, path) => {
      let original = path->List.reverse
      let sum = original->List.reduce(0, (total, curr) => total + curr)
      sum === targetSum ? list{List.toArray(original), ...acc} : acc
    })
    ->List.toArray
  }
}

/**
             5
           /   \
         4       8
        /       / \
      11      13   4
     /  \           / \
    7    2         5   1
 */
let tree1 = Some(
  TreeNode.make(
    ~val=5,
    ~left=Some(
      TreeNode.make(
        ~val=4,
        ~left=Some(
          TreeNode.make(
            ~val=11,
            ~left=Some(TreeNode.make(~val=7)),
            ~right=Some(TreeNode.make(~val=2)),
          ),
        ),
      ),
    ),
    ~right=Some(
      TreeNode.make(
        ~val=8,
        ~left=Some(TreeNode.make(~val=13)),
        ~right=Some(
          TreeNode.make(
            ~val=4,
            ~left=Some(TreeNode.make(~val=5)),
            ~right=Some(TreeNode.make(~val=1)),
          ),
        ),
      ),
    ),
  ),
)
let r1 = pathSum_II(tree1, 22)
Console.log2("r1: ", r1) // [[5,4,11,2],[5,8,4,5]]

/**
     1
   /   \
  2     3
 */
let tree2 = Some(
  TreeNode.make(~val=1, ~left=Some(TreeNode.make(~val=2)), ~right=Some(TreeNode.make(~val=3))),
)
let r2 = pathSum_II(tree2, 5)
Console.log2("r2: ", r2) // []
