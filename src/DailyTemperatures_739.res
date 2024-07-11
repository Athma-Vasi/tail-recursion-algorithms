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

// using monotonic decreasing stack
// T(n) = O(n)
// S(n) = O(n)
// INCORRECT

let dailyTemperatures = (temps: array<int>) => {
  let length = Array.length(temps)

  let rec loop = (accumulator: array<int>, currentDay: int, stack: array<int>) => {
    let currentTemp = switch temps->Array.get(currentDay) {
    | None => Int32.min_int
    | Some(num) => num
    }
    let stackLength = Array.length(stack)

    currentDay === length - 1
      ? accumulator
      : switch stackLength === 0 {
        | true => {
            let newStack = stack->Array.concat([currentDay])
            loop(accumulator, currentDay + 1, newStack)
          }
        | false => {
            let prevDay = switch stack->Array.get(stackLength - 1) {
            | None => Int32.min_int
            | Some(num) => num
            }

            let previousTemp = switch temps->Array.get(prevDay) {
            | None => Int32.min_int
            | Some(num) => num
            }

            switch currentTemp > previousTemp {
            | true => {
                let rec stackLoop = (accumulator: array<int>, stack: array<int>) => {
                  let stackLen = Array.length(stack)
                  let prevDay = switch stack->Array.get(stackLen - 1) {
                  | None => Int32.min_int
                  | Some(num) => num
                  }
                  let sliced =
                    stack->Array.slice(~start=0, ~end=stackLength - 1)->Array.concat([currentDay])

                  let daysToWait = currentDay - prevDay
                  let newAcc =
                    accumulator->Array.mapWithIndex((num, idx) =>
                      idx === currentDay - 1 ? daysToWait : num
                    )

                  switch stackLen === 0 || daysToWait < 0 {
                  | true => [newAcc, sliced]
                  | false => stackLoop(newAcc, sliced)
                  }
                }

                let tuple = stackLoop(accumulator, stack)
                let newAcc = switch tuple->Array.get(0) {
                | None => []
                | Some(arr) => arr
                }
                let newStack = switch tuple->Array.get(1) {
                | None => []
                | Some(arr) => arr
                }

                loop(newAcc, currentDay + 1, newStack)
              }
            | false => {
                let newStack = stack->Array.concat([currentDay])
                loop(accumulator, currentDay + 1, newStack)
              }
            }
          }
        }
  }

  let accumulator = Array.make(~length=Array.length(temps), 0)
  loop(accumulator, 0, [])
}

let t1 = [73, 74, 75, 71, 69, 72, 76, 73]
let r1 = dailyTemperatures(t1)
Console.log2("r1", r1)

let t2 = [30, 40, 50, 60]
let r2 = dailyTemperatures(t2)
Console.log2("r2", r2)

let t3 = [30, 60, 90]
let r3 = dailyTemperatures(t3)
Console.log2("r3", r3)
