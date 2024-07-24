// incorrect
let dailyTemperatures = (temps: array<int>) => {
  let length = Array.length(temps)

  let rec updateDaysAndStack = (
    daysToWait: array<int>,
    monoDecrStack: array<int>,
    indexToPush: int,
  ) => {
    let stackLength = Array.length(monoDecrStack)

    let numToPush = switch temps->Array.get(indexToPush) {
    | None => Int32.min_int
    | Some(num) => num
    }

    let prevMinimumIndex = switch monoDecrStack->Array.at(-1) {
    | None => -1
    | Some(num) => num
    }
    let prevMinimum = switch temps->Array.get(prevMinimumIndex) {
    | None => Int32.max_int
    | Some(num) => num
    }

    Console.log("--updateDaysAndStack--")
    Console.log2("daysToWait: ", daysToWait)
    Console.log2("indexToPush: ", indexToPush)
    Console.log2("numToPush: ", numToPush)
    Console.log2("prevMinimumIndex: ", prevMinimumIndex)
    Console.log2("prevMinimum: ", prevMinimum)

    switch numToPush < prevMinimum {
    | true => (daysToWait, monoDecrStack->Array.concat([indexToPush]))
    | false => {
        daysToWait->Array.set(prevMinimumIndex, indexToPush - prevMinimumIndex)

        updateDaysAndStack(
          daysToWait,
          monoDecrStack->Array.slice(~start=1, ~end=stackLength),
          indexToPush,
        )
      }
    }
  }

  let rec loop = (daysToWait: array<int>, monoDecrStack: array<int>, index: int) => {
    switch index === length {
    | true => daysToWait
    | false => {
        Console.log("--loop--")
        Console.log2("daysToWait: ", daysToWait)
        Console.log2("monoDecrStack: ", monoDecrStack)

        let (updatedDaysToWait, updatedMonoDecrStack) = updateDaysAndStack(
          daysToWait,
          monoDecrStack,
          index,
        )

        Console.log2("updatedDaysToWait: ", updatedDaysToWait)
        Console.log2("updatedMonoDecrStack: ", updatedMonoDecrStack)

        loop(updatedDaysToWait, updatedMonoDecrStack, index + 1)
      }
    }
  }

  loop(Array.make(~length, -1), [], 0)
}

let t1 = [73, 74, 75, 71, 69, 72, 76, 73]
let r1 = dailyTemperatures(t1)
Console.log2("r1: ", r1)

// T(n) = O(n^2)
// S(n) = O(n)

// let dailyTemperatures = (temps: array<int>): array<int> => {
//   let length = Array.length(temps)

//   let rec anchorLoop = (accumulator: array<int>, anchor: int) => {
//     let anchorNum = switch temps->Array.get(anchor) {
//     | None => Int32.min_int
//     | Some(num) => num
//     }

//     let rec explorerLoop = (explorer: int) => {
//       let explorerNum = switch temps->Array.get(explorer) {
//       | None => Int32.min_int
//       | Some(num) => num
//       }

//       switch explorer === length {
//       | true => 0
//       | false =>
//         switch explorerNum > anchorNum {
//         | true => explorer - anchor
//         | false => explorerLoop(explorer + 1)
//         }
//       }
//     }

//     let daysToWait = explorerLoop(anchor + 1)
//     switch anchor === length - 1 {
//     | true => accumulator->Array.concat([0])
//     | false => anchorLoop(accumulator->Array.concat([daysToWait]), anchor + 1)
//     }
//   }

//   switch length > 1 {
//   | true => anchorLoop([], 0)
//   | false => [0]
//   }
// }
