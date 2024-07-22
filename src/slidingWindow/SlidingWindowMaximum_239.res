// T(n) = O(n)
// S(n) = O(m + n)

let slidingWindowMaximum = (numbers: array<int>, windowSize: int) => {
  let length = Array.length(numbers)

  let rec updateMonoDecrStack = (monoDecrStack: array<int>, numToPush: int): array<int> => {
    let stackLength = Array.length(monoDecrStack)

    let prevMaximum = switch monoDecrStack->Array.at(-1) {
    | None => Int32.min_int
    | Some(num) => num
    }

    switch prevMaximum > numToPush {
    | true => monoDecrStack
    | false =>
      switch stackLength < 1 {
      | true => monoDecrStack->Array.concat([numToPush])
      | false =>
        updateMonoDecrStack(monoDecrStack->Array.slice(~start=0, ~end=stackLength - 1), numToPush)
      }
    }
  }

  let rec expandWindow = (monoDecrStack: array<int>, index: int): array<int> => {
    switch index === windowSize {
    | true => monoDecrStack
    | false => {
        let currentNum = switch numbers->Array.get(index) {
        | None => Int32.min_int + 1
        | Some(num) => num
        }

        let updatedMonoDecrStack = updateMonoDecrStack(monoDecrStack, currentNum)
        expandWindow(updatedMonoDecrStack, index + 1)
      }
    }
  }

  let rec loop = (
    ~maximumArrays: array<int>,
    ~monoDecrStack: array<int>,
    ~leftIndex: int,
    ~rightIndex: int,
  ) => {
    switch rightIndex === length {
    | true => maximumArrays
    | false => {
        let rightIncludedNum = switch numbers->Array.get(rightIndex) {
        | None => Int32.min_int
        | Some(num) => num
        }

        let rightUpdatedMonoDecrStack = updateMonoDecrStack(monoDecrStack, rightIncludedNum)

        let maximum = switch rightUpdatedMonoDecrStack->Array.at(-1) {
        | None => Int32.min_int
        | Some(num) => num
        }

        loop(
          ~maximumArrays=maximumArrays->Array.concat([maximum]),
          ~monoDecrStack=rightUpdatedMonoDecrStack,
          ~leftIndex=leftIndex + 1,
          ~rightIndex=rightIndex + 1,
        )
      }
    }
  }

  let updatedMonoDecrStack = expandWindow([], 0)
  let maximum = switch updatedMonoDecrStack->Array.at(-1) {
  | None => Int32.min_int
  | Some(num) => num
  }
  let maximumArrays = []->Array.concat([maximum])

  loop(~maximumArrays, ~monoDecrStack=updatedMonoDecrStack, ~leftIndex=1, ~rightIndex=windowSize)
}

Console.log(slidingWindowMaximum([1, 3, -1, -3, 5, 3, 6, 7], 3))
Console.log(slidingWindowMaximum([1], 1))
