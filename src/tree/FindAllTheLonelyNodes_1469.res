// T(n) = O(n)
// S(n) = O(n)

type nodeKind = Leaf | Branch

let findAllTheLonelyNodes = (root: option<TreeNode.t<int>>) => {
  // Simulates a postorder traversal manually, collecting values and node kinds
  let rec postorderTraverse = (
    // rpnStack stores (node value, node kind), accumulated in postorder
    rpnStack: list<(int, nodeKind)>,
    // workingStack is the traversal stack used for simulating postorder
    workingStack: list<TreeNode.t<int>>,
  ) => {
    switch workingStack {
    // Base case: done traversing
    | list{} => rpnStack
    // Continue processing next node
    | list{node, ...rest} => {
        let {left, right, val} = node

        switch (left, right) {
        // Leaf: push (val, Leaf)
        | (None, None) => postorderTraverse(list{(val, Leaf), ...rpnStack}, rest)
        // Only right child exists: mark node as Branch and traverse right child
        | (None, Some(rightNode)) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{rightNode, ...rest})
        // Only left child exists: mark node as Branch and traverse left child
        | (Some(leftNode), None) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{leftNode, ...rest})
        // Both children exist: mark as Branch, and traverse both (right before left to simulate postorder)
        | (Some(leftNode), Some(rightNode)) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{rightNode, leftNode, ...rest})
        }
      }
    }
  }

  let rec processRPNStack = (
    // The postorder-ordered stack of (val, kind)
    rpnStack: list<(int, nodeKind)>,
    // Accumulates lonely node values
    lonelies: list<int>,
    // Used to simulate evaluation of nodes as subtrees are processed
    evaluationStack: list<(int, nodeKind)>,
  ) => {
    switch rpnStack {
    // Base case: done processing all nodes
    | list{} => lonelies
    // Continue processing current node
    | list{(val, nodeKind), ...rest} =>
      switch nodeKind {
      // Push leaf onto evaluation stack
      | Leaf => processRPNStack(rest, lonelies, list{(val, nodeKind), ...evaluationStack})

      // For branch nodes, try to find lonely leaf children
      | Branch => {
          // Separate most recent leaf nodes from the rest of evaluation stack
          let (topLeaves, sliced) = evaluationStack->List.reduce((list{}, list{}), (acc, curr) => {
            let (topLeaves, sliced) = acc
            let (val, nodeKind) = curr

            switch nodeKind {
            | Leaf => (list{val, ...topLeaves}, sliced)
            | Branch => (topLeaves, list{curr, ...sliced})
            }
          })

          // Restore stack order
          let original = sliced->List.reverse
          // Push current branch node onto updated eval stack
          let newEvalStack = list{(val, nodeKind), ...original}

          // If exactly one leaf was associated with this branch, it's lonely
          switch List.size(topLeaves) === 1 {
          | true => {
              let topLeaf = topLeaves->List.head->Option.mapOr(Int32.min_int, v => v)
              // If we’re processing the root node last, skip adding
              processRPNStack(
                rest,
                List.size(rpnStack) === 1 ? lonelies : list{topLeaf, ...lonelies},
                newEvalStack,
              )
            }
          | false =>
            // No lonely node here — continue
            processRPNStack(rest, lonelies, newEvalStack)
          }
        }
      }
    }
  }

  // Main entry point: start traversal and process result
  switch root {
  | None => []
  | Some(node) =>
    postorderTraverse(list{}, list{node})->processRPNStack(list{}, list{})->List.toArray
  }
}

/**
    1
   / \
  2   3
   \
    4
 */
let tree1 = Some(
  TreeNode.make(
    ~val=1,
    ~left=Some(TreeNode.make(~val=2, ~right=Some(TreeNode.make(~val=4)))),
    ~right=Some(TreeNode.make(~val=3)),
  ),
)
let r1 = findAllTheLonelyNodes(tree1)
Console.log2("r1: ", r1) // [4]
