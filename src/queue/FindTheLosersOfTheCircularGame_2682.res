let findTheLosersOfTheCircularGame = (n: int, k: int) => {
  let makeRange = (max: int) => {
    let rec collect = (range: array<int>, num: int) => {
      switch num === max + 1 {
      | true => range
      | false => collect(range->Array.concat([num]), num + 1)
      }
    }

    collect([], 1)
  }

  let players = makeRange(n)

  let rec play = (count: int, scoreTable: Map.t<int, int>, index: int) => {
    let player = players->Array.at(index)->Option.mapOr(0, p => p)
    let score = scoreTable->Map.get(player)->Option.mapOr(0, s => s)

    Console.log("\n")
    Console.log("--play--")
    Console.log2("count: ", count)
    Console.log2("scoreTable: ", scoreTable)
    Console.log2("index: ", index)
    Console.log2("player: ", player)
    Console.log2("score: ", score)

    switch score + 1 > 1 {
    | true => scoreTable
    | false => {
        let iterationCount = count * k
        let newIndex = Float.mod(Int.toFloat(index + iterationCount), Int.toFloat(n))->Int.fromFloat
        let newPlayer = players->Array.at(newIndex)->Option.mapOr(0, p => p)
        let newScore = scoreTable->Map.get(newPlayer)->Option.mapOr(1, s => s + 1)
        scoreTable->Map.set(newPlayer, newScore)

        Console.log("\n")
        Console.log2("iterationCount: ", iterationCount)
        Console.log2("newIndex: ", newIndex)
        Console.log2("newPlayer: ", newPlayer)
        Console.log2("newScore: ", newScore)
        Console.log2("scoreTable: ", scoreTable)

        play(count + 1, scoreTable, newIndex)
      }
    }
  }

  let scoreTable = players->Array.reduceWithIndex(Map.make(), (acc, _num, index) => {
    acc->Map.set(index + 1, 0)
    acc
  })

  play(0, scoreTable, 0)
}

let n1 = 5
let k1 = 2
let r1 = findTheLosersOfTheCircularGame(n1, k1)
Console.log2("r1: ", r1)
