// T(n) = O(n)
// S(n) = O(n)

let twoSumIV_BST = (root: option<TreeNode.t<int>>, k: int) => {
  let rec breadthFirstTraverse = (
    exists: bool,
    values: Set.t<int>,
    stack: list<TreeNode.t<int>>,
  ) => {
    switch exists {
    | true => true
    | false =>
      switch stack {
      | list{} => exists
      | list{popped, ...rest} => {
          let {left, right, val} = popped
          let exists_ = values->Set.has(k - val)
          values->Set.add(val)

          switch (left, right) {
          | (None, None) => breadthFirstTraverse(exists_, values, rest)
          | (None, Some(rightNode)) =>
            breadthFirstTraverse(exists_, values, list{rightNode, ...rest})
          | (Some(leftNode), None) => breadthFirstTraverse(exists_, values, list{leftNode, ...rest})
          | (Some(leftNode), Some(rightNode)) =>
            breadthFirstTraverse(exists_, values, list{leftNode, rightNode, ...rest})
          }
        }
      }
    }
  }

  switch root {
  | None => false
  | Some(node) => breadthFirstTraverse(false, Set.make(), list{node})
  }
}

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15))
let r1 = twoSumIV_BST(Some(root1), 10)
Console.log2("r1: ", r1) // true
let r2 = twoSumIV_BST(Some(root1), 20)
Console.log2("r2: ", r2) // true
let r3 = twoSumIV_BST(Some(root1), 30)
Console.log2("r3: ", r3) // false
