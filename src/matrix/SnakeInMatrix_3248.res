// T(n) = O(n)
// S(n) = O(1)

type commands = UP | RIGHT | DOWN | LEFT

let snakeInMatrix = (n: int, commandsArr: array<commands>) => {
  let rec loop = (position: int, x: int, y: int, index: int) => {
    switch index === Array.length(commandsArr) {
    | true => position
    | false => {
        let command = switch commandsArr->Array.at(index) {
        | None => UP
        | Some(c) => c
        }

        switch command {
        | UP => loop(position - n, x, y + 1, index + 1)
        | RIGHT => loop(position + 1, x + 1, y, index + 1)
        | DOWN => loop(position + n, x, y - 1, index + 1)
        | LEFT => loop(position - 1, x - 1, y, index + 1)
        }
      }
    }
  }

  loop(0, 0, 0, 0)
}

let n1 = 2
let c1 = [RIGHT, DOWN]
let r1 = snakeInMatrix(n1, c1)
Console.log2("r1: ", r1) // 3

let n2 = 3
let c2 = [DOWN, RIGHT, UP]
let r2 = snakeInMatrix(n2, c2)
Console.log2("r2: ", r2) // 1
