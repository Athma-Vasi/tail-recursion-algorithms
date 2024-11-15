// T(n) = O(n)
// S(n) = O(n)

let shortestSubarrayToBeRemovedToMakeArraySorted = (arr: array<int>) => {
  let rec updateMonoIncrStack = (
    subarray: array<int>,
    monoIncrStack: array<int>,
    numToPush: int,
  ) => {
    let prevNum = switch monoIncrStack->Array.at(-1) {
    | None => -1
    | Some(n) => n
    }

    switch prevNum <= numToPush {
    | true => (subarray, monoIncrStack->Array.concat([numToPush]))
    | false =>
      updateMonoIncrStack(
        subarray->Array.concat([prevNum]),
        monoIncrStack->Array.slice(~start=0, ~end=Array.length(monoIncrStack) - 1),
        numToPush,
      )
    }
  }

  let rec loop = (subarray: array<int>, monoIncrStack: array<int>, index: int) => {
    switch index === Array.length(arr) {
    | true => Array.length(subarray)
    | false => {
        let numToPush = switch arr->Array.at(index) {
        | None => -1
        | Some(n) => n
        }

        let (updatedSubarray, updatedMonoIncrStack) = updateMonoIncrStack(
          subarray,
          monoIncrStack,
          numToPush,
        )

        loop(updatedSubarray, updatedMonoIncrStack, index + 1)
      }
    }
  }

  loop([], [], 0)
}

let a1 = [1, 2, 3, 10, 4, 2, 3, 5]
let r1 = shortestSubarrayToBeRemovedToMakeArraySorted(a1)
Console.log2("r1: ", r1) // 3

let a2 = [5, 4, 3, 2, 1]
let r2 = shortestSubarrayToBeRemovedToMakeArraySorted(a2)
Console.log2("r2: ", r2) // 4

let a3 = [1, 2, 3]
let r3 = shortestSubarrayToBeRemovedToMakeArraySorted(a3)
Console.log2("r3: ", r3) // 0
