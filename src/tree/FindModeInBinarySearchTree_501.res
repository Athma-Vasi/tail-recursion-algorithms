// T(n) = O(n)
// S(n) = O(n)

let findModeInBinarySearchTree = (root: option<TreeNode.t<int>>) => {
  let rec inorderTraverse = (
    freqTable: Map.t<int, int>,
    curr: option<TreeNode.t<int>>,
    stack: list<TreeNode.t<int>>,
  ) => {
    switch curr {
    | None =>
      switch stack {
      | list{} => freqTable
      | list{popped, ...rest} => {
          let {right, val} = popped
          let freq = freqTable->Map.get(val)->Option.mapOr(1, f => f + 1)
          freqTable->Map.set(val, freq)

          inorderTraverse(freqTable, right, rest)
        }
      }
    | Some(node) => inorderTraverse(freqTable, node.left, list{node, ...stack})
    }
  }

  let sorted =
    inorderTraverse(Map.make(), root, list{})
    ->Map.entries
    ->Core__Iterator.toArray
    ->Array.toSorted(((_val1, freq1), (_val2, freq2)) => Int.compare(freq2, freq1))
  let (_val, maxFreq) = sorted->Array.at(0)->Option.mapOr((Int32.min_int, 0), t => t)
  maxFreq === 1
    ? []
    : sorted->Array.reduce([], (acc, (val, freq)) => {
        freq === maxFreq ? acc->Array.concat([val]) : acc
      })
}

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = Some(TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15)))
let r1 = findModeInBinarySearchTree(root1)
Console.log2("r1: ", r1)

let symmetric = Some(
  TreeNode.make(
    ~val=1,
    ~left=Some(
      TreeNode.make(~val=2, ~left=Some(TreeNode.make(~val=3)), ~right=Some(TreeNode.make(~val=4))),
    ),
    ~right=Some(
      TreeNode.make(~val=2, ~left=Some(TreeNode.make(~val=4)), ~right=Some(TreeNode.make(~val=3))),
    ),
  ),
)
let r2 = findModeInBinarySearchTree(symmetric)
Console.log2("r2: ", r2)
