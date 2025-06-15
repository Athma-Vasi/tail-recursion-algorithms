// T(n) = O(n)
// S(n) = O(n)

let naryTreePreorderTraversal = (root: option<NaryTreeNode.t<int>>) => {
  let rec traverse = (result: list<int>, stack: list<NaryTreeNode.t<int>>): array<int> => {
    switch stack {
    | list{} => result->List.reverse->List.toArray
    | list{popped, ...rest} => {
        let {branches, val} = popped
        let newResult = list{val, ...result}

        switch branches {
        | None => traverse(newResult, rest)
        | Some(branches) =>
          traverse(
            newResult,
            branches->List.reduce(rest, (acc, branch) => {
              list{branch, ...acc}
            }),
          )
        }
      }
    }
  }

  switch root {
  | None => []
  | Some(node) => traverse(list{}, list{node})
  }
}

let n2 = NaryTreeNode.make(~val=2)
let n3 = NaryTreeNode.make(~val=3)
let n5 = NaryTreeNode.make(~val=5)
let n4 = NaryTreeNode.make(~val=4, ~branches=Some(list{n5}))
let root1 = NaryTreeNode.make(~val=1, ~branches=Some(list{n2, n3, n4}))
let r1 = naryTreePreorderTraversal(Some(root1))
Console.log2("r1: ", r1) // [ 1, 4, 5, 3, 2 ]
