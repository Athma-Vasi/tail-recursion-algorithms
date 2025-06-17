// T(n) = O(n)
// S(n) = O(n)

type nodeKind = Leaf | Branch

let evaluateBooleanBinaryTree = (root: option<TreeNode.t<int>>) => {
  // Postorder traversal: returns a reverse polish notation (RPN)-style list
  // Each element includes the value and whether it's a leaf or branch node
  let rec postorderTraverse = (
    rpnStack: list<(int, nodeKind)>,
    workingStack: list<TreeNode.t<int>>,
  ) => {
    switch workingStack {
    | list{} => rpnStack // Traversal complete
    | list{top, ...rest} => {
        let {left, right, val} = top

        // Determine kind of node (Leaf or Branch), and continue traversal
        switch (left, right) {
        | (None, None) => postorderTraverse(list{(val, Leaf), ...rpnStack}, rest)
        | (None, Some(rightNode)) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{rightNode, ...rest})
        | (Some(leftNode), None) =>
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{leftNode, ...rest})
        | (Some(leftNode), Some(rightNode)) =>
          // For postorder, push right first, then left
          postorderTraverse(list{(val, Branch), ...rpnStack}, list{rightNode, leftNode, ...rest})
        }
      }
    }
  }

  // Evaluate the postorder RPN stack
  let rec processResults = (evaluationStack: list<bool>, rpnStack: list<(int, nodeKind)>) => {
    switch rpnStack {
    | list{} => evaluationStack // Evaluation complete
    | list{(val, nodeKind), ...rest} =>
      switch nodeKind {
      // Convert leaf value (0/1) to bool and push to eval stack
      | Leaf => processResults(list{val === 1, ...evaluationStack}, rest)
      | Branch => {
          let (leftBool, rightBool) = evaluationStack->List.reduceWithIndex((false, false), (
            acc,
            curr,
            idx,
          ) => {
            let (leftBool, rightBool) = acc
            idx === 1 ? (curr, rightBool) : (leftBool, curr)
          })

          // Apply operator: 2 = OR, anything else (assumed 3) = AND
          switch val {
          | 2 => processResults(list{leftBool || rightBool}, rest)
          | _ => processResults(list{leftBool && rightBool}, rest)
          }
        }
      }
    }
  }

  switch root {
  | None => false // Empty tree evaluates to false
  | Some(node) =>
    processResults(list{}, postorderTraverse(list{}, list{node}))
    ->List.head
    ->Option.mapOr(false, h => h) // Get result or default to false
  }
}

// Leaf nodes (values are 0 or 1)
let l1 = TreeNode.make(~val=0)
let l2 = TreeNode.make(~val=1)
let l3 = TreeNode.make(~val=1)
let l4 = TreeNode.make(~val=0)

// Intermediate level with operators (2 = OR, 3 = AND)
let andNode = TreeNode.make(~val=3, ~left=Some(l1), ~right=Some(l2)) // 0 AND 1 = 0
let orNode = TreeNode.make(~val=2, ~left=Some(l3), ~right=Some(l4)) // 1 OR 0 = 1

// Root node (AND of the two subtrees)
let root = Some(TreeNode.make(~val=3, ~left=Some(andNode), ~right=Some(orNode))) // 0 AND 1 = 0
let r1 = evaluateBooleanBinaryTree(root)
Console.log2("r1: ", r1)
