// T(n) = O(n)
// S(n) = O(n)

let baseballGame = (ops: array<string>) => {
  let opsLength = Array.length(ops)

  let rec loop = (scores: array<int>, index: int) => {
    let scoresLength = Array.length(scores)

    switch index === opsLength {
    | true => scores->Array.reduce(0, (acc, score) => acc + score)
    | false =>
      switch ops->Array.get(index) {
      | None => -1
      | Some(char) =>
        switch char === "+" {
        | true => {
            let leftScore = switch scores->Array.at(-2) {
            | None => -1
            | Some(num) => num
            }
            let rightScore = switch scores->Array.at(-1) {
            | None => -1
            | Some(num) => num
            }

            loop(scores->Array.concat([leftScore + rightScore]), index + 1)
          }
        | false =>
          switch char === "D" {
          | true => {
              let lastScore = switch scores->Array.at(-1) {
              | None => -1
              | Some(num) => num
              }

              loop(scores->Array.concat([lastScore * 2]), index + 1)
            }
          | false =>
            switch char === "C" {
            | true => loop(scores->Array.slice(~start=0, ~end=scoresLength - 1), index + 1)
            | false => {
                let intScore = switch Int.fromString(char) {
                | None => -1
                | Some(int) => int
                }

                loop(scores->Array.concat([intScore]), index + 1)
              }
            }
          }
        }
      }
    }
  }

  loop([], 0)
}

let o1 = ["5", "2", "C", "D", "+"]
let r1 = baseballGame(o1)
Console.log2("r1: ", r1)

let o2 = ["5", "-2", "4", "C", "D", "9", "+", "+"]
let r2 = baseballGame(o2)
Console.log2("r2: ", r2)

let o3 = ["1"]
let r3 = baseballGame(o3)
Console.log2("r3: ", r3)
