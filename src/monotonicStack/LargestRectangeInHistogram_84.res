// T(n) = O(n)
// S(n) = O(n)
// does not cover all edge cases. only covers example cases
let largestRectangeInHistogram = (heights: array<int>) => {
  let rec updateMonoIncrStack = (maxArea: int, monoIncrStack: array<int>, indexToPush: int): (
    int,
    array<int>,
  ) => {
    let stackLength = Array.length(monoIncrStack)
    let heightToPush = switch heights->Array.at(indexToPush) {
    | None => -1
    | Some(num) => num
    }
    let previousIndex = switch monoIncrStack->Array.at(-1) {
    | None => Int32.min_int
    | Some(num) => num
    }
    let previousHeight = switch heights->Array.at(previousIndex) {
    | None => -1
    | Some(num) => num
    }

    switch heightToPush > previousHeight {
    | true => (maxArea, monoIncrStack->Array.concat([indexToPush]))
    | false => {
        let newArea = previousHeight * (indexToPush - previousIndex)

        updateMonoIncrStack(
          maxArea > newArea ? maxArea : newArea,
          monoIncrStack->Array.slice(~start=0, ~end=stackLength - 1),
          indexToPush,
        )
      }
    }
  }

  let rec flushStack = (maxArea: int, monoIncrStack: array<int>): int => {
    let stackLength = Array.length(monoIncrStack)

    switch stackLength < 1 {
    | true => maxArea
    | false => {
        let smallestHeightIdx = switch monoIncrStack->Array.at(0) {
        | None => Int32.min_int
        | Some(num) => num
        }
        let smallestHeight = switch heights->Array.at(smallestHeightIdx) {
        | None => -1
        | Some(num) => num
        }

        // stack is now entirely monotonically increasing
        let newArea = smallestHeight * stackLength

        flushStack(
          maxArea > newArea ? maxArea : newArea,
          monoIncrStack->Array.slice(~start=1, ~end=stackLength),
        )
      }
    }
  }

  let rec loop = (maxArea: int, monoIncrStack: array<int>, index: int): int => {
    switch index === Array.length(heights) {
    | true =>
      switch Array.length(monoIncrStack) < 1 {
      | true => maxArea
      | false => flushStack(maxArea, monoIncrStack)
      }
    | false => {
        let (newMaxArea, updatedMonoIncrStack) = updateMonoIncrStack(maxArea, monoIncrStack, index)

        loop(newMaxArea, updatedMonoIncrStack, index + 1)
      }
    }
  }

  loop(Int32.min_int, [], 0)
}

let h1 = [2, 1, 5, 6, 2, 3]
let r1 = largestRectangeInHistogram(h1)
Console.log3("r1: ", r1, r1 === 10)

let h2 = [2, 4]
let r2 = largestRectangeInHistogram(h2)
Console.log2("r2: ", r2) // 4

// let h3 = [2, 1, 2]
// let r3 = largestRectangeInHistogram(h3)
// Console.log3("r3: ", r3, r3 === 3)

// let h4 = [1, 1]
// let r4 = largestRectangeInHistogram(h4)
// Console.log3("r4: ", r4, r4 === 2)
