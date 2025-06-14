// T(n) = O(n)
// S(n) = O(w) where w is the maximum number of nodes at any level

let maximumDepthOfNaryTree = (root: option<NaryTreeNode.t<int>>) => {
  let rec breadthFirstTraverse = (maxDepth: int, stack: list<(NaryTreeNode.t<int>, int)>) => {
    // Base case: if the stack is empty, return the maximum depth found.
    switch stack {
    | list{} => maxDepth

    // Process the next node and its depth from the stack.
    | list{(poppedNode, depth), ...rest} => {
        let {branches} = poppedNode

        // Update the maximum depth if the current node's depth is greater.
        let newMaxDepth = maxDepth > depth ? maxDepth : depth

        // If the node has no children, continue traversal with the rest of the stack.
        switch branches {
        | None => breadthFirstTraverse(newMaxDepth, rest)

        // If the node has children, add each child to the stack with depth + 1.
        | Some(branches) => {
            // Accumulate new stack by prepending (child, depth + 1) for each child.
            // Children are added in reverse order for efficiency since List is singly-linked.
            let newStack = branches->List.reduce(rest, (acc, branch) => {
              list{(branch, depth + 1), ...acc}
            })

            // Recurse with the updated stack and max depth.
            breadthFirstTraverse(newMaxDepth, newStack)
          }
        }
      }
    }
  }

  // If the tree is empty, return depth 0.
  // Otherwise, start traversal from the root node at depth 1.
  switch root {
  | None => 0
  | Some(node) => breadthFirstTraverse(0, list{(node, 1)})
  }
}

let n2 = NaryTreeNode.make(~val=2)
let n3 = NaryTreeNode.make(~val=3)
let n5 = NaryTreeNode.make(~val=5)
let n4 = NaryTreeNode.make(~val=4, ~branches=Some(list{n5}))
let root1 = NaryTreeNode.make(~val=1, ~branches=Some(list{n2, n3, n4}))
let r1 = maximumDepthOfNaryTree(Some(root1))
Console.log2("r1: ", r1)
