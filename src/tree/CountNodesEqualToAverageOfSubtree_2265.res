// T(n) = O(n)
// S(n) = O(n)

type nodeKind = Leaf | Branch

let countNodesEqualToAverageOfSubtree = (root: option<TreeNode.t<int>>) => {
  // First phase: simulate a postorder traversal (Right-to-Left) and generate an RPN-style stack
  let rec postorderTraverse = (
    rpnStack: list<(int, nodeKind)>, // holds (node value, nodeKind) in postorder
    workingStack: list<TreeNode.t<int>>,
  ) => {
    switch workingStack {
    | list{} => rpnStack // Base case: done with traversal
    | list{top, ...rest} => {
        let {left, right, val} = top

        switch (left, right) {
        | (None, None) => postorderTraverse(list{(val, Leaf), ...rpnStack}, rest)
        | (None, Some(rightNode)) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{rightNode, ...rest})
        | (Some(leftNode), None) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{leftNode, ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{rightNode, leftNode, ...rest})
        }
      }
    }
  }

  // Second phase: process the RPN-style postorder list to evaluate subtree sums/counts
  let rec processResults = (
    count: int,
    evaluationStack: list<(int, int)>, // holds (cumulativeSum, subTreeCount) for subtrees
    rpnStack: list<(int, nodeKind)>,
  ) => {
    switch rpnStack {
    | list{} => count // Done processing all nodes
    | list{(val, nodeKind), ...rest} =>
      switch nodeKind {
      // For leaf nodes, add value as a trivial subtree (sum = val, count = 0)
      | Leaf => processResults(count, list{(val, 0), ...evaluationStack}, rest)
      // For branch nodes, pop left and right subtree values from the evaluation stack
      | Branch => {
          // Reduce the evaluation stack to extract up to two child subtree evaluations
          let (leftValue, rightValue, restEval) = evaluationStack->List.reduceWithIndex(
            (None, None, list{}),
            (acc, curr, idx) => {
              let (leftValue, rightValue, restEval) = acc

              idx === 0
                ? (leftValue, Some(curr), restEval)
                : idx === 1
                ? (Some(curr), rightValue, restEval)
                : (leftValue, rightValue, list{curr, ...restEval})
            },
          )

          switch (leftValue, rightValue) {
          // If somehow both children are missing, skip
          | (None, None) => count
          // Case: only one child exists
          | (None, Some((rightVal, subTreeCount))) => {
              let sum = rightVal + val
              let totalSubTreeCount = subTreeCount + 1 // include current node
              let isAverage = sum / totalSubTreeCount === val

              processResults(
                isAverage ? count + totalSubTreeCount : count,
                list{(sum, totalSubTreeCount + 1), ...restEval},
                rest,
              )
            }
          | (Some((leftVal, subTreeCount)), None) => {
              let sum = leftVal + val
              let totalSubTreeCount = subTreeCount + 1
              let isAverage = sum / totalSubTreeCount === val

              processResults(
                isAverage ? count + totalSubTreeCount : count,
                list{(sum, totalSubTreeCount + 1), ...restEval},
                rest,
              )
            }
          // Case: both left and right children exist
          | (Some((leftVal, leftSubTreeCount)), Some((rightVal, rightSubTreeCount))) => {
              let sum = leftVal + rightVal + val
              // Fixing leaf counts to ensure each contributes at least 1 node
              let subTreeCount =
                (leftSubTreeCount === 0 ? 1 : leftSubTreeCount) + (
                  rightSubTreeCount === 0 ? 1 : rightSubTreeCount + 1
                )
              let isAverage = sum / subTreeCount === val

              processResults(
                isAverage ? count + subTreeCount : count,
                list{(sum, subTreeCount + 1), ...restEval},
                rest,
              )
            }
          }
        }
      }
    }
  }

  // Entry point: if root is present, run both phases
  switch root {
  | None => 0 // No tree to process
  | Some(node) => processResults(0, list{}, postorderTraverse(list{}, list{node}))
  }
}

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(
  ~val=15,
  ~left=Some(TreeNode.make(~val=13)),
  ~right=Some(TreeNode.make(~val=18)),
)
let root1 = Some(TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15)))
let r1 = countNodesEqualToAverageOfSubtree(root1)
Console.log2("r1: ", r1) // 7

let n4 = TreeNode.make(
  ~val=4,
  ~left=Some(TreeNode.make(~val=2)),
  ~right=Some(TreeNode.make(~val=6)),
)

let n12 = TreeNode.make(
  ~val=12,
  ~left=Some(TreeNode.make(~val=10)),
  ~right=Some(TreeNode.make(~val=14)),
)

let root2 = Some(TreeNode.make(~val=8, ~left=Some(n4), ~right=Some(n12)))

let r2 = countNodesEqualToAverageOfSubtree(root2)
Console.log2("r2: ", r2) // 7
