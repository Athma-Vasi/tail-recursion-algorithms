// T(n) = O(n)
// S(n) = O(n)

let naryTreePostorderTraversal = (root: option<NaryTreeNode.t<int>>) => {
  let rec traverse = (resultStack: list<int>, workingStack: list<NaryTreeNode.t<int>>) => {
    switch workingStack {
    | list{} => resultStack->List.toArray
    | list{top, ...rest} => {
        let {branches, val} = top
        let newResultStack = list{val, ...resultStack}

        switch branches {
        | None => traverse(newResultStack, rest)
        | Some(branches) =>
          traverse(
            newResultStack,
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
let r1 = naryTreePostorderTraversal(Some(root1))
Console.log2("r1: ", r1) // [ 2, 3, 5, 4, 1 ]
