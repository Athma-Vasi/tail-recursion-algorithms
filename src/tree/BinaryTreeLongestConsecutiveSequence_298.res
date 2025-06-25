let binaryTreeLongestConsecutiveSequence = (root: option<TreeNode.t<int>>) => {
  // Performs a preorder traversal to find all downward consecutive sequences in the binary tree
  let rec preorderTraverse = (
    sequences: list<list<int>>, // Accumulates all discovered consecutive sequences
    // Stack of (node, current sequence path)
    stack: list<(TreeNode.t<int>, list<int>)>,
  ) => {
    switch stack {
    | list{} => sequences // All nodes processed
    | list{(node, sequence), ...rest} => {
        let {left, right, val} = node

        // Determine whether current node continues the previous sequence
        let (newSequence, newSequences) = switch sequence {
        | list{} => (list{val}, sequences) // Start of a new sequence
        | list{prevMaxSeq, ...restSeq} =>
          switch val - prevMaxSeq === 1 {
          | true => (list{val, prevMaxSeq, ...restSeq}, sequences) // Continue sequence
          // Restart sequence, store the old one
          | false => (list{val}, list{list{prevMaxSeq, ...restSeq}, ...sequences})
          }
        }

        // Continue traversal based on existing children
        switch (left, right) {
        | (None, None) =>
          // Leaf node: store completed sequence
          preorderTraverse(list{newSequence, ...newSequences}, rest)
        | (None, Some(rightNode)) =>
          preorderTraverse(newSequences, list{(rightNode, newSequence), ...rest})
        | (Some(leftNode), None) =>
          preorderTraverse(newSequences, list{(leftNode, newSequence), ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          preorderTraverse(
            newSequences,
            list{(leftNode, newSequence), (rightNode, newSequence), ...rest},
          )
        }
      }
    }
  }

  switch root {
  | None => -1
  | Some(node) =>
    // Start traversal and sort all collected sequences by length (descending)
    preorderTraverse(list{}, list{(node, list{})})
    ->List.toArray
    ->Array.toSorted((seq1, seq2) => Int.compare(seq2->List.size, seq1->List.size))
    ->Array.at(0) // Pick the longest one
    ->Option.mapOr(-1, longestSeq => List.size(longestSeq))
  }
}

/**
    1
     \
      3
     / \
    2   4
         \
          5
 */
let tree1 = Some(
  TreeNode.make(
    ~val=1,
    ~right=Some(
      TreeNode.make(
        ~val=3,
        ~left=Some(TreeNode.make(~val=2)),
        ~right=Some(TreeNode.make(~val=4, ~right=Some(TreeNode.make(~val=5)))),
      ),
    ),
  ),
)
let r1 = binaryTreeLongestConsecutiveSequence(tree1)
Console.log2("r1: ", r1) // 3

/**
    2
     \
      3
     /
    2
   /
  1
 */
let tree2 = Some(
  TreeNode.make(
    ~val=2,
    ~right=Some(
      TreeNode.make(~val=3, ~left=Some(TreeNode.make(~val=2, ~left=Some(TreeNode.make(~val=1))))),
    ),
  ),
)
let r2 = binaryTreeLongestConsecutiveSequence(tree2)
Console.log2("r2: ", r2) // 2
