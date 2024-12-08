// T(n) = O(n)
// S(n) = O(n)

let findTheWinnerOfTheCircularGame = (n: int, k: int) => {
  let makeRange = (start: int, end: int) => {
    let rec collect = (range: array<int>, limit: int) =>
      limit > end ? range : collect(range->Array.concat([limit]), limit + 1)

    collect([], start)
  }

  let removeNum = (nums, index) => nums->Array.filterWithIndex((_num, idx) => idx !== index)

  let rec play = (remaining: array<int>, startIndex: int) => {
    let length = Array.length(remaining)
    switch length <= 1 {
    | true =>
      switch remaining->Array.at(0) {
      | None => 0
      | Some(p) => p
      }
    | false => {
        let newIndex =
          startIndex + k - 1 >= length
            ? Float.mod(Int.toFloat(startIndex + k - 1), Int.toFloat(length))
            : Int.toFloat(startIndex + k - 1)
        let removed = removeNum(remaining, Int.fromFloat(newIndex))

        play(removed, Int.fromFloat(newIndex))
      }
    }
  }

  play(makeRange(1, n), 0)
}

let n1 = 5
let k1 = 2
let r1 = findTheWinnerOfTheCircularGame(n1, k1)
Console.log2("r1: ", r1) // 3

let n2 = 6
let k2 = 5
let r2 = findTheWinnerOfTheCircularGame(n2, k2)
Console.log2("r2: ", r2) // 1
