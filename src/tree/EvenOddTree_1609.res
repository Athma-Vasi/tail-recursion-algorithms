// T(n) = O(n)
// S(n) = O(n)

type monotonicKind = Increasing | Decreasing
type intKind = Even | Odd

let evenOddTree = (root: option<TreeNode.t<int>>) => {
  let rec breadthFirstTraverse = (
    levelValuesTable: Map.t<int, list<int>>,
    queue: array<(TreeNode.t<int>, int)>,
  ) => {
    switch queue->Array.at(0) {
    | None => levelValuesTable
    | Some((node, level)) => {
        let rest = queue->Array.sliceToEnd(~start=1)
        let {left, right, val} = node
        let values =
          levelValuesTable->Map.get(level)->Option.mapOr(list{val}, vals => list{val, ...vals})
        levelValuesTable->Map.set(level, values)

        switch (left, right) {
        | (None, None) => breadthFirstTraverse(levelValuesTable, rest)
        | (None, Some(rightNode)) => {
            rest->Array.push((rightNode, level + 1))
            breadthFirstTraverse(levelValuesTable, rest)
          }
        | (Some(leftNode), None) => {
            rest->Array.push((leftNode, level + 1))
            breadthFirstTraverse(levelValuesTable, rest)
          }
        | (Some(leftNode), Some(rightNode)) => {
            rest->Array.push((leftNode, level + 1))
            rest->Array.push((rightNode, level + 1))
            breadthFirstTraverse(levelValuesTable, rest)
          }
        }
      }
    }
  }

  let checkIsMonotonic = (values: list<int>, kind: monotonicKind) => {
    values
    ->List.reduceWithIndex(Set.make(), (acc, curr, idx) => {
      switch values->List.get(idx - 1) {
      | None => acc
      | Some(prev) => {
          acc->Set.add(
            switch kind {
            | Increasing => curr > prev
            | Decreasing => curr < prev
            },
          )
          acc
        }
      }
    })
    ->Set.has(false)
      ? false
      : true
  }

  let checkAllElementsEvenOrOdd = (values: list<int>, kind: intKind) => {
    values
    ->List.reduce(Set.make(), (acc, curr) => {
      acc->Set.add(
        switch kind {
        | Even => Float.mod(Int.toFloat(curr), 2.0) === 0.0
        | Odd => Float.mod(Int.toFloat(curr), 2.0) !== 0.0
        },
      )
      acc
    })
    ->Set.has(false)
      ? false
      : true
  }

  switch root {
  | None => false
  | Some(node) =>
    breadthFirstTraverse(Map.make(), [(node, 0)])
    ->Map.entries
    ->Array.fromIterator
    ->Array.reduce(Set.make(), (acc, (level, values)) => {
      let isLevelEven = Float.mod(Int.toFloat(level), 2.0) === 0.0
      let original = values->List.reverse

      switch isLevelEven {
      | true => {
          let areAllElementsOdd = checkAllElementsEvenOrOdd(original, Odd)
          let isMonotonicallyIncreasing = checkIsMonotonic(original, Increasing)
          acc->Set.add(areAllElementsOdd && isMonotonicallyIncreasing)
          acc
        }
      | false => {
          let areAllElementsEven = checkAllElementsEvenOrOdd(original, Even)
          let isMonotonicallyDecreasing = checkIsMonotonic(original, Decreasing)
          acc->Set.add(areAllElementsEven && isMonotonicallyDecreasing)
          acc
        }
      }
    })
    ->Set.has(false)
      ? false
      : true
  }
}

/**
                    1
                   / \
                 10   4
                 /    / \
               3    7   9
              / \   /    \
            12  8  6      2
                      
 */
let tree1: option<TreeNode.t<int>> = Some(
  TreeNode.make(
    ~val=1,
    ~left=Some(
      TreeNode.make(
        ~val=10,
        ~left=Some(
          TreeNode.make(
            ~val=3,
            ~left=Some(TreeNode.make(~val=12)),
            ~right=Some(TreeNode.make(~val=8)),
          ),
        ),
      ),
    ),
    ~right=Some(
      TreeNode.make(
        ~val=4,
        ~left=Some(TreeNode.make(~val=7, ~left=Some(TreeNode.make(~val=6)))),
        ~right=Some(TreeNode.make(~val=9, ~right=Some(TreeNode.make(~val=2)))),
      ),
    ),
  ),
)
let r1 = evenOddTree(tree1)
Console.log2("r1: ", r1) // true

/**
          5
       /   \
      4     2
     / \   /
    3   3 7
 */
let tree2: option<TreeNode.t<int>> = Some(
  TreeNode.make(
    ~val=5,
    ~left=Some(
      TreeNode.make(~val=4, ~left=Some(TreeNode.make(~val=3)), ~right=Some(TreeNode.make(~val=3))),
    ),
    ~right=Some(TreeNode.make(~val=2, ~left=Some(TreeNode.make(~val=7)))),
  ),
)
let r2 = evenOddTree(tree2)
Console.log2("r2: ", r2) // false

/**
         5
      /   \
     9     1
    / \   /
   3   5 7
 */
let tree3: option<TreeNode.t<int>> = Some(
  TreeNode.make(
    ~val=5,
    ~left=Some(
      TreeNode.make(~val=9, ~left=Some(TreeNode.make(~val=3)), ~right=Some(TreeNode.make(~val=5))),
    ),
    ~right=Some(TreeNode.make(~val=1, ~left=Some(TreeNode.make(~val=7)))),
  ),
)
let r2 = evenOddTree(tree3)
Console.log2("r2: ", r2) // false
