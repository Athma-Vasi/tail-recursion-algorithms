// T(n) = O(n)
// S(n) = O(n)

let subtreeOfAnotherTree = (root: option<TreeNode.t<int>>, subRoot: option<TreeNode.t<int>>) => {
  let rec preorderTraverse = (serialized: string, workingStack: list<TreeNode.t<int>>) => {
    switch workingStack {
    | list{} => serialized
    | list{top, ...rest} => {
        let {left, right, val} = top
        let serialized_ = serialized->String.concat("->" ++ Int.toString(val))

        switch (left, right) {
        | (None, None) => preorderTraverse(serialized_, rest)
        | (None, Some(rightNode)) => preorderTraverse(serialized_, list{rightNode, ...rest})
        | (Some(leftNode), None) => preorderTraverse(serialized_, list{leftNode, ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          preorderTraverse(serialized_, list{leftNode, rightNode, ...rest})
        }
      }
    }
  }

  switch (root, subRoot) {
  | (None, None) => false
  | (None, Some(_subRootNode)) => false
  | (Some(_rootNode), None) => false
  | (Some(rootNode), Some(subRootNode)) => {
      let rootSerialized = preorderTraverse(String.make(), list{rootNode})
      let subRootSerialized = preorderTraverse(String.make(), list{subRootNode})
      rootSerialized->String.includes(subRootSerialized)
    }
  }
}

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = Some(TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15)))
let r1 = subtreeOfAnotherTree(root1, Some(n5))
Console.log2("r1: ", r1)
