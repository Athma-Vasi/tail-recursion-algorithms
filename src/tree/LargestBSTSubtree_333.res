// T(n) = O(n)
// S(n) = O(n)

// Define the kind of node: either a Leaf or a Branch (i.e., has children)
type nodeKind = Leaf | Branch

let largestBSTSubtree = (root: option<TreeNode.t<int>>) => {
  // Postorder traversal that builds:
  // - rpnStack: list of nodes with their kinds in Reverse Polish Notation (postorder)
  // - nodeLeavesCountTable: mapping of each node to how many leaf children it has
  let rec postorderTraverse = (
    rpnStack: list<(TreeNode.t<int>, nodeKind)>,
    nodeLeavesCountTable: Map.t<TreeNode.t<int>, int>,
    workingStack: list<TreeNode.t<int>>,
  ) => {
    switch workingStack {
    // If no more nodes to process, return the stack and table
    | list{} => (rpnStack, nodeLeavesCountTable)
    | list{top, ...rest} => {
        let {left, right} = top

        switch (left, right) {
        // Case 1: No children → it's a leaf
        | (None, None) => {
            nodeLeavesCountTable->Map.set(top, 0)
            postorderTraverse(list{(top, Leaf), ...rpnStack}, nodeLeavesCountTable, rest)
          }

        // Case 2: Only right child → one leaf child
        | (None, Some(rightNode)) => {
            nodeLeavesCountTable->Map.set(top, 1)
            postorderTraverse(
              list{(top, Branch), ...rpnStack},
              nodeLeavesCountTable,
              list{rightNode, ...rest},
            )
          }

        // Case 3: Only left child → one leaf child
        | (Some(leftNode), None) => {
            nodeLeavesCountTable->Map.set(top, 1)
            postorderTraverse(
              list{(top, Branch), ...rpnStack},
              nodeLeavesCountTable,
              list{leftNode, ...rest},
            )
          }

        // Case 4: Both children → two leaf children
        | (Some(leftNode), Some(rightNode)) => {
            nodeLeavesCountTable->Map.set(top, 2)
            postorderTraverse(
              list{(top, Branch), ...rpnStack},
              nodeLeavesCountTable,
              list{rightNode, leftNode, ...rest}, // right first to preserve postorder
            )
          }
        }
      }
    }
  }

  // Evaluate the RPN stack to determine the size of the largest BST subtree
  let rec processRPNStack = (
    rpnStack: list<(TreeNode.t<int>, nodeKind)>,
    nodeLeavesCountTable: Map.t<TreeNode.t<int>, int>,
    largestSize: int,
    // Each entry: (value, node kind, subtree size)
    evaluationStack: list<(int, nodeKind, int)>,
  ) => {
    switch rpnStack {
    // Final result: return largest BST size found
    | list{} => largestSize

    // Process top node
    | list{(node, nodeKind), ...rest} =>
      switch nodeKind {
      // Leaf node: subtree size is 0
      | Leaf =>
        processRPNStack(
          rest,
          nodeLeavesCountTable,
          largestSize,
          list{(node.val, nodeKind, 0), ...evaluationStack},
        )

      // Branch node: evaluate whether its children form a valid BST
      | Branch => {
          let leavesCount = nodeLeavesCountTable->Map.get(node)->Option.getOr(0)

          // Extract up to `leavesCount` most recent leaf nodes from evaluationStack
          let (topLeaves, sliced) = evaluationStack->List.reduceWithIndex((list{}, list{}), (
            acc,
            curr,
            idx,
          ) => {
            let (topLeaves, sliced) = acc
            let (val, nodeKind, subtreeSize) = curr

            idx < leavesCount && nodeKind === Leaf
              ? (list{(val, subtreeSize), ...topLeaves}, sliced)
              : (topLeaves, list{(val, nodeKind, subtreeSize), ...sliced})
          })

          // Extract values and cumulative subtree size from topLeaves
          let (leavesValues, countWithLeaves) = switch List.size(topLeaves) === 2 {
          | true =>
            topLeaves->List.reduceReverse((list{}, 0), (acc, (currVal, countSoFar)) => {
              let (leavesValues, countWithLeaves) = acc
              ({list{currVal, ...leavesValues}}, countWithLeaves + countSoFar)
            })
          | false => (list{}, 0)
          }

          // Check if node.val sits between its children to qualify as BST
          let newLargestSize = switch leavesValues {
          | list{} => largestSize
          | list{smaller, ...restSizes} =>
            switch restSizes {
            | list{} => largestSize
            | list{larger, ..._nextRestLeaves} =>
              switch smaller < node.val && node.val < larger {
              | true => {
                  let newBSTSize = countWithLeaves + 1
                  largestSize > newBSTSize ? largestSize : newBSTSize
                }
              | false => largestSize
              }
            }
          }

          // Push new computed subtree onto evaluation stack and continue
          processRPNStack(
            rest,
            nodeLeavesCountTable,
            newLargestSize,
            list{(node.val, Leaf, newLargestSize), ...sliced},
          )
        }
      }
    }
  }

  // Main begins here
  switch root {
  // Edge case: empty tree
  | None => -1

  // Process root using traversal then evaluation
  | Some(node) => {
      let (rpnStack, nodeLeavesCountTable) = postorderTraverse(list{}, Map.make(), list{node})
      processRPNStack(rpnStack, nodeLeavesCountTable, 0, list{})
    }
  }
}

/**
        10
       /  \
      5    15
     / \     \
    1   8     7

 */
let tree1 = Some(
  TreeNode.make(
    ~val=10,
    ~left=Some(
      TreeNode.make(~val=5, ~left=Some(TreeNode.make(~val=1)), ~right=Some(TreeNode.make(~val=8))),
    ),
    ~right=Some(TreeNode.make(~val=15, ~right=Some(TreeNode.make(~val=7)))),
  ),
)
let r1 = largestBSTSubtree(tree1)
Console.log2("r1: ", r1) // 3
