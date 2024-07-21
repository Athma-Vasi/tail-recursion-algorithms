// T(n) = O(n)
// S(n) = O(m + (n - m + 1)) where m <= n

let slidingWindowMaximum = (numbers: array<int>, windowSize: int) => {
  let length = Array.length(numbers)

  let rec updateMonoIncrStack = (monoIncrStack: array<int>, numToPush: int): array<int> => {
    let stackLength = Array.length(monoIncrStack)

    let prevMaximum = switch monoIncrStack->Array.at(-1) {
    | None => Int32.min_int
    | Some(num) => num
    }

    switch prevMaximum > numToPush {
    | true => monoIncrStack
    | false =>
      switch Array.length(monoIncrStack) < 1 {
      | true => monoIncrStack->Array.concat([numToPush]) // push
      | false =>
        updateMonoIncrStack(monoIncrStack->Array.slice(~start=0, ~end=stackLength - 1), numToPush) // pop
      }
    }
  }

  let rec expandWindow = (monoIncrStack: array<int>, index: int): array<int> => {
    switch index === windowSize {
    | true => monoIncrStack
    | false => {
        let currentNum = switch numbers->Array.get(index) {
        | None => Int32.min_int + 1
        | Some(num) => num
        }

        let updatedMonoIncrStack = updateMonoIncrStack(monoIncrStack, currentNum)
        expandWindow(updatedMonoIncrStack, index + 1)
      }
    }
  }

  let rec loop = (
    ~maximumArrays: array<int>,
    ~monoIncrStack: array<int>,
    ~leftIndex: int,
    ~rightIndex: int,
  ) => {
    switch rightIndex === length {
    | true => maximumArrays
    | false => {
        let leftExcludedNum = switch numbers->Array.get(leftIndex - 1) {
        | None => Int32.min_int
        | Some(num) => num
        }

        let rightIncludedNum = switch numbers->Array.get(rightIndex) {
        | None => Int32.min_int
        | Some(num) => num
        }

        let leftUpdatedMonoIncrStack = updateMonoIncrStack(monoIncrStack, leftExcludedNum)
        let rightUpdatedMonoIncrStack = updateMonoIncrStack(
          leftUpdatedMonoIncrStack,
          rightIncludedNum,
        )

        let maximum = switch rightUpdatedMonoIncrStack->Array.at(-1) {
        | None => Int32.min_int
        | Some(num) => num
        }

        loop(
          ~maximumArrays=maximumArrays->Array.concat([maximum]),
          ~monoIncrStack=rightUpdatedMonoIncrStack,
          ~leftIndex=leftIndex + 1,
          ~rightIndex=rightIndex + 1,
        )
      }
    }
  }

  let updatedMonoIncrStack = expandWindow([], 0)
  let maximum = switch updatedMonoIncrStack->Array.at(-1) {
  | None => Int32.min_int
  | Some(num) => num
  }
  let maximumArrays = []->Array.concat([maximum])

  loop(~maximumArrays, ~monoIncrStack=updatedMonoIncrStack, ~leftIndex=1, ~rightIndex=windowSize)
}

Console.log(slidingWindowMaximum([1, 3, -1, -3, 5, 3, 6, 7], 3))
Console.log(slidingWindowMaximum([1], 1))
