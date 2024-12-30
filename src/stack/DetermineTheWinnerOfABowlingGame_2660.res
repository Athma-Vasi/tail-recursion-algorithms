// T(n) = O(n)
// S(n) = O(n)

let calculateScore = (player: array<int>) => {
  let rec calculate = (score: int, stack: array<int>, index: int) => {
    switch index === Array.length(player) {
    | true => score
    | false => {
        let pinsHit = player->Array.at(index)->Option.mapOr(-1, p => p)
        let prevPinsHit = stack->Array.at(-1)->Option.mapOr(-1, p => p)
        let prevPrevPinsHit = stack->Array.at(-2)->Option.mapOr(-1, p => p)

        calculate(
          prevPinsHit === 10 || prevPrevPinsHit === 10 ? score + pinsHit * 2 : score + pinsHit,
          stack->Array.concat([pinsHit]),
          index + 1,
        )
      }
    }
  }

  calculate(0, [], 0)
}

let determineTheWinnerOfABowlingGame = (player1: array<int>, player2: array<int>) => {
  let score1 = calculateScore(player1)
  let score2 = calculateScore(player2)

  score1 > score2 ? 1 : score1 < score2 ? 2 : 0
}

let p1 = [5, 10, 3, 2]
let p11 = [6, 5, 7, 3]
let r1 = determineTheWinnerOfABowlingGame(p1, p11)
Console.log2("r1: ", r1) // 1

let p2 = [3, 5, 7, 6]
let p22 = [8, 10, 10, 2]
let r2 = determineTheWinnerOfABowlingGame(p2, p22)
Console.log2("r2: ", r2) // 2

let p3 = [2, 3]
let p33 = [4, 1]
let r3 = determineTheWinnerOfABowlingGame(p3, p33)
Console.log2("r3: ", r3) // 0

let p4 = [1, 1, 1, 10, 10, 10, 10]
let p44 = [10, 10, 10, 10, 1, 1, 1]
let r4 = determineTheWinnerOfABowlingGame(p4, p44)
Console.log2("r4: ", r4) // 2
