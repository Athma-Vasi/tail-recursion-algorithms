// T(n) = O(n)
// S(n) = O(n)
// code by me, comments by llm

let maximumDepthOfBinaryTree = (root: option<TreeNode.t<int>>) => {
  // Recursive function that performs a breadth-first traversal (BFS)
  // to find the maximum depth of the binary tree.
  // `maxDepth`: the current maximum depth seen so far
  // `queue`: a list of (node, depth) pairs to process
  let rec breadthFirstTraverse = (maxDepth: int, queue: list<(TreeNode.t<int>, int)>) => {
    switch queue {
    // If the queue is empty, we've visited all nodes, so return the max depth
    | list{} => maxDepth

    // Otherwise, deconstruct the next node and its depth from the queue
    | list{(poppedNode, level), ...rest} => {
        let {left, right} = poppedNode

        // Update maxDepth if the current level is deeper
        let newMaxDepth = maxDepth > level ? maxDepth : level

        // Depending on whether the node has children, add them to the queue with incremented level
        switch (left, right) {
        // Leaf node — no children to enqueue
        | (None, None) => breadthFirstTraverse(newMaxDepth, rest)

        // Only right child exists — enqueue it
        | (None, Some(rightNode)) =>
          breadthFirstTraverse(newMaxDepth, rest->List.concat(list{(rightNode, level + 1)}))

        // Only left child exists — enqueue it
        | (Some(leftNode), None) =>
          breadthFirstTraverse(newMaxDepth, rest->List.concat(list{(leftNode, level + 1)}))

        // Both children exist — enqueue both
        | (Some(leftNode), Some(rightNode)) =>
          breadthFirstTraverse(
            newMaxDepth,
            rest->List.concat(list{(leftNode, level + 1), (rightNode, level + 1)}),
          )
        }
      }
    }
  }

  // If the tree is empty, the depth is 0; otherwise, start BFS with the root at depth 1
  switch root {
  | None => 0
  | Some(node) => breadthFirstTraverse(0, list{(node, 1)})
  }
}

let n5 = TreeNode.make(
  ~val=5,
  ~left=Some(TreeNode.make(~val=3)),
  ~right=Some(TreeNode.make(~val=7)),
)
let n15 = TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=18)))
let root1 = TreeNode.make(~val=10, ~left=Some(n5), ~right=Some(n15))
let r1 = maximumDepthOfBinaryTree(Some(root1))
Console.log2("r1: ", r1)
