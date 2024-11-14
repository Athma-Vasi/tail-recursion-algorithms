// T(n) = O(n)
// S(n) = O(1)

let findWinningPlayerInCoinGame = (x: int, y: int) => {
  let rec loop = (player: string, xRemainder: int, yRemainder: int) => {
    switch (xRemainder < 2 && yRemainder < 4) || yRemainder < 4 {
    | true => player === "Alice" ? "Bob" : "Alice"
    | false => loop(player === "Alice" ? "Bob" : "Alice", xRemainder - 1, yRemainder - 4)
    }
  }

  loop("Alice", x, y)
}

let x1 = 2
let y1 = 7
let r1 = findWinningPlayerInCoinGame(x1, y1)
Console.log2("r1: ", r1)

let x2 = 4
let y2 = 11
let r2 = findWinningPlayerInCoinGame(x2, y2)
Console.log2("r2: ", r2)
