// T(n) = O(n^2)
// S(n) = O(n)

let dailyTemperatures = (temps: array<int>): array<int> => {
  let length = Array.length(temps)

  let rec anchorLoop = (accumulator: array<int>, anchor: int) => {
    let anchorNum = switch temps->Array.get(anchor) {
    | None => Int32.min_int
    | Some(num) => num
    }

    let rec explorerLoop = (explorer: int) => {
      let explorerNum = switch temps->Array.get(explorer) {
      | None => Int32.min_int
      | Some(num) => num
      }

      switch explorer === length {
      | true => 0
      | false =>
        switch explorerNum > anchorNum {
        | true => explorer - anchor
        | false => explorerLoop(explorer + 1)
        }
      }
    }

    let daysToWait = explorerLoop(anchor + 1)
    switch anchor === length - 1 {
    | true => accumulator->Array.concat([0])
    | false => anchorLoop(accumulator->Array.concat([daysToWait]), anchor + 1)
    }
  }

  switch length > 1 {
  | true => anchorLoop([], 0)
  | false => [0]
  }
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
