// T(m, n) = O(max(m, n)) -- we visit each node once in both trees
// S(m, n) = O(max(m, n)) -- stack size + map space in worst case

let checkIfTwoExpressionTreesAreEquivalent = (
  root1: option<TreeNode.t<string>>,
  root2: option<TreeNode.t<string>>,
) => {
  // Performs an iterative inorder traversal of both trees in parallel.
  // Accumulates frequency counts of node values into two maps.
  let rec inorderTraverse = (
    charsFreqs1: Map.t<string, int>, // frequency map for tree 1
    charsFreqs2: Map.t<string, int>, // frequency map for tree 2
    curr1: option<TreeNode.t<string>>, // current node in tree 1
    curr2: option<TreeNode.t<string>>, // current node in tree 2
    stack1: list<TreeNode.t<string>>, // traversal stack for tree 1
    // traversal stack for tree 2
    stack2: list<TreeNode.t<string>>,
  ) => {
    switch (curr1, curr2) {
    // Both current nodes are None, need to process top of stacks
    | (None, None) =>
      switch (stack1, stack2) {
      // Both stacks empty: traversal complete
      | (list{}, list{}) => (charsFreqs1, charsFreqs2)

      // Only stack2 has elements: pop and process
      | (list{}, list{top2, ...rest2}) => {
          let {right, val} = top2
          let freq2 = charsFreqs2->Map.get(val)->Option.mapOr(1, f => f + 1)
          charsFreqs2->Map.set(val, freq2)

          inorderTraverse(charsFreqs1, charsFreqs2, curr1, right, stack1, rest2)
        }

      // Only stack1 has elements: pop and process
      | (list{top1, ...rest1}, list{}) => {
          let {right, val} = top1
          let freq1 = charsFreqs1->Map.get(val)->Option.mapOr(1, f => f + 1)
          charsFreqs1->Map.set(val, freq1)

          inorderTraverse(charsFreqs1, charsFreqs2, right, curr2, rest1, stack2)
        }

      // Both stacks non-empty: pop both and process their values
      | (list{top1, ...rest1}, list{top2, ...rest2}) => {
          let freq1 = charsFreqs1->Map.get(top1.val)->Option.mapOr(1, f => f + 1)
          charsFreqs1->Map.set(top1.val, freq1)

          let freq2 = charsFreqs2->Map.get(top2.val)->Option.mapOr(1, f => f + 1)
          charsFreqs2->Map.set(top2.val, freq2)

          inorderTraverse(charsFreqs1, charsFreqs2, top1.right, top2.right, rest1, rest2)
        }
      }

    // Tree2 has a current node; go left
    | (None, Some(node2)) =>
      inorderTraverse(charsFreqs1, charsFreqs2, None, node2.left, stack1, list{node2, ...stack2})

    // Tree1 has a current node; go left
    | (Some(node1), None) =>
      inorderTraverse(charsFreqs1, charsFreqs2, node1.left, None, list{node1, ...stack1}, stack2)

    // Both trees have a current node; go left on both
    | (Some(node1), Some(node2)) =>
      inorderTraverse(
        charsFreqs1,
        charsFreqs2,
        node1.left,
        node2.left,
        list{node1, ...stack1},
        list{node2, ...stack2},
      )
    }
  }

  // Start traversal and retrieve frequency maps
  let (charsFreqs1, charsFreqs2) = inorderTraverse(
    Map.make(),
    Map.make(),
    root1,
    root2,
    list{},
    list{},
  )

  // Compare both frequency maps: if any mismatch, return false
  charsFreqs1
  ->Map.entries
  ->Array.fromIterator
  ->Array.reduce(Set.make(), (acc, (char1, freq1)) => {
    switch charsFreqs2->Map.get(char1) {
    | None => {
        acc->Set.add(false)
        acc
      }
    | Some(freq2) => {
        acc->Set.add(freq1 === freq2)
        acc
      }
    }
  })
  ->Set.has(false)
    ? false // Mismatch found
    : true // All frequencies match
}

// Tree structure:
//       +
//      / \
//     a   +
//        / \
//       b   c
let tree1 = Some(
  TreeNode.make(
    ~val="+",
    ~left=Some(TreeNode.make(~val="a")),
    ~right=Some(
      TreeNode.make(
        ~val="+",
        ~left=Some(TreeNode.make(~val="b")),
        ~right=Some(TreeNode.make(~val="c")),
      ),
    ),
  ),
)
/**
        +
       / \
      +   b
     / \
    c   a
 */
let tree2 = Some(
  TreeNode.make(
    ~val="+",
    ~left=Some(
      TreeNode.make(
        ~val="+",
        ~left=Some(TreeNode.make(~val="c")),
        ~right=Some(TreeNode.make(~val="a")),
      ),
    ),
    ~right=Some(TreeNode.make(~val="b")),
  ),
)

let r1 = checkIfTwoExpressionTreesAreEquivalent(tree1, tree2)
Console.log2("r1: ", r1) // true

/**
        +
       / \
      a   +
         / \
        b   c
 */
let tree3 = Some(
  TreeNode.make(
    ~val="+",
    ~left=Some(TreeNode.make(~val="a")),
    ~right=Some(
      TreeNode.make(
        ~val="+",
        ~left=Some(TreeNode.make(~val="b")),
        ~right=Some(TreeNode.make(~val="c")),
      ),
    ),
  ),
)

/**
        +
       / \
      +   b
     / \
    d   a
 */
let tree4 = Some(
  TreeNode.make(
    ~val="+",
    ~left=Some(
      TreeNode.make(
        ~val="+",
        ~left=Some(TreeNode.make(~val="d")),
        ~right=Some(TreeNode.make(~val="a")),
      ),
    ),
    ~right=Some(TreeNode.make(~val="b")),
  ),
)
let r2 = checkIfTwoExpressionTreesAreEquivalent(tree3, tree4)
Console.log2("r2: ", r2) // false
