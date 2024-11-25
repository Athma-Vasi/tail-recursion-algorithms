// T(n) = O(n)
// S(n) = O(n)

let findThePeaks = (mountain: array<int>) => {
  let rec loop = (indexes: array<int>, stack: array<int>, index: int) => {
    switch index === Array.length(mountain) {
    | true => indexes
    | false => {
        let currPeak = switch mountain->Array.at(index) {
        | None => 0
        | Some(p) => p
        }
        let prevPeak = switch stack->Array.at(-1) {
        | None => 0
        | Some(p) => p
        }
        let prevPrevPeak = switch stack->Array.at(-2) {
        | None => 0
        | Some(p) => p
        }
        let updatedStack = stack->Array.concat([currPeak])

        switch prevPeak > prevPrevPeak && prevPeak > currPeak {
        | true => loop(indexes->Array.concat([index - 1]), updatedStack, index + 1)
        | false => loop(indexes, updatedStack, index + 1)
        }
      }
    }
  }

  loop([], [], 0)
}

let m1 = [2, 4, 4]
let r1 = findThePeaks(m1)
Console.log2("r1: ", r1) // []

let m2 = [1, 4, 3, 8, 5]
let r2 = findThePeaks(m2)
Console.log2("r2: ", r2) // [1, 3]
