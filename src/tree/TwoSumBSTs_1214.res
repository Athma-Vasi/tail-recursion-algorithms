// T(m, n) = O(m + n)
// S(m, n) = O(m + n)

let twoSumBSTs = (root1: option<TreeNode.t<int>>, root2: option<TreeNode.t<int>>, target: int) => {
  // Performs iterative in-order traversal and collects node values in a list.
  // It builds the list in reverse order and then reverses it at the end for correct ascending order.
  let rec inorderTraverse = (
    resultStack: list<int>, // Accumulator for node values
    curr: option<TreeNode.t<int>>, // Current node being processed
    // Stack used to simulate recursion
    workingStack: list<TreeNode.t<int>>,
  ) => {
    switch curr {
    | None =>
      switch workingStack {
      // All nodes processed, return reversed result to get ascending order
      | list{} => resultStack->List.reverse
      // Pop from working stack and process right subtree next
      | list{popped, ...rest} =>
        // Prepend current value // Move to right subtree
        inorderTraverse(list{popped.val, ...resultStack}, popped.right, rest)
      }
    | Some(node) =>
      // Continue traversing left subtree
      inorderTraverse(resultStack, node.left, list{node, ...workingStack})
    }
  }

  // Determines whether any pair of values from the two sorted lists sums to the target
  let sumExists = (sorted1: list<int>, sorted2: list<int>) => {
    let length2 = List.size(sorted2)

    let rec find = (exists: bool, leftIdx: int, rightIdx: int) => {
      switch rightIdx < 0 || leftIdx > length2 {
      | true => exists // Out of bounds: stop recursion
      | false =>
        switch (sorted1->List.get(leftIdx), sorted2->List.get(rightIdx)) {
        | (Some(leftNum), Some(rightNum)) =>
          // If sum matches target, return true
          leftNum + rightNum === target || (
              // Otherwise move left or right pointer depending on the sum
              leftNum + rightNum < target
                ? find(exists, leftIdx + 1, rightIdx)
                : find(exists, leftIdx, rightIdx - 1)
            )

        | _ => // One of the indices is out of bounds — defensive fallback
          exists
        }
      }
    }

    // Start two-pointer search from ends of both lists
    find(false, 0, length2 - 1)
  }

  // Run in-order traversal on both trees to get sorted values
  let sorted1 = inorderTraverse(list{}, root1, list{})
  let sorted2 = inorderTraverse(list{}, root2, list{})

  // Check if any pair sums to target
  sumExists(sorted1, sorted2)
}

/**
    2
   / \
  1   4
 */
let tree1 = Some(
  TreeNode.make(~val=2, ~left=Some(TreeNode.make(~val=1)), ~right=Some(TreeNode.make(~val=4))),
)

/**
    1
   / \
  0   3
 */
let tree2 = Some(
  TreeNode.make(~val=1, ~left=Some(TreeNode.make(~val=0)), ~right=Some(TreeNode.make(~val=3))),
)
let r1 = twoSumBSTs(tree1, tree2, 5)
Console.log2("r1: ", r1) // true

/**
     0
    / \
 -10   10
 */
let tree3 = Some(
  TreeNode.make(~val=0, ~left=Some(TreeNode.make(~val=-10)), ~right=Some(TreeNode.make(~val=10))),
)

/**
      5
     / \
    1   7
   / \
  0   2
 */
let tree4 = Some(
  TreeNode.make(
    ~val=5,
    ~left=Some(
      TreeNode.make(~val=1, ~left=Some(TreeNode.make(~val=0)), ~right=Some(TreeNode.make(~val=2))),
    ),
    ~right=Some(TreeNode.make(~val=7)),
  ),
)
let r2 = twoSumBSTs(tree3, tree4, 18)
Console.log2("r2: ", r2) // false
