// T(n) = O(n)
// S(n) = O(n)

let findNumberOfWinningPlayers = (_n: int, picks: array<(int, int)>) => {
  let rec loop = (winners: int, table: Map.t<int, Map.t<int, int>>, index: int) => {
    switch index === Array.length(picks) {
    | true =>
      table
      ->Map.entries
      ->Core__Iterator.toArray
      ->Array.reduce(0, (acc, (player, colorCounts)) => {
        colorCounts
        ->Map.entries
        ->Core__Iterator.toArray
        ->Array.reduce(acc, (acc_, (_color, colorCount)) => {
          colorCount === player + 1 ? acc_ + 1 : acc_
        })
      })
    | false => {
        let (player, ballColor) = switch picks->Array.at(index) {
        | None => (-1, -1)
        | Some(tuple) => tuple
        }
        let colorCounts = switch table->Map.get(player) {
        | None => Map.make()
        | Some(m) => m
        }
        let colorCount = switch colorCounts->Map.get(ballColor) {
        | None => 0
        | Some(c) => c
        }
        colorCounts->Map.set(ballColor, colorCount + 1)
        table->Map.set(player, colorCounts)

        loop(winners, table, index + 1)
      }
    }
  }

  loop(0, Map.make(), 0)
}

let n1 = 4
let p1 = [(0, 0), (1, 0), (1, 0), (2, 1), (2, 1), (2, 0)]
let r1 = findNumberOfWinningPlayers(n1, p1)
Console.log2("r1: ", r1)

let n2 = 5
let p2 = [(1, 1), (1, 2), (1, 3), (1, 4)]
let r2 = findNumberOfWinningPlayers(n2, p2)
Console.log2("r2: ", r2)

let n3 = 5
let p3 = [(1, 1), (2, 4), (2, 4), (2, 4)]
let r3 = findNumberOfWinningPlayers(n3, p3)
Console.log2("r3: ", r3)
