// T(n) = O(n)
// S(n) = O(1)

let furthestPointFromOrigin = (moves: string) => {
  let rec travel = (result: int, distance: int, index: int) => {
    switch index === String.length(moves) {
    | true => result < 0 ? result * -1 + distance : result + distance
    | false => {
        let move = moves->String.charAt(index)

        switch move {
        | "L" => travel(result - 1, distance, index + 1)
        | "R" => travel(result + 1, distance, index + 1)
        | _ => travel(result, distance + 1, index + 1)
        }
      }
    }
  }

  travel(0, 0, 0)
}

let m1 = "L_RL__R"
let r1 = furthestPointFromOrigin(m1)
Console.log2("r1: ", r1) // 3

let m2 = "_R__LL_"
let r2 = furthestPointFromOrigin(m2)
Console.log2("r2: ", r2) // 5

let m3 = "_______"
let r3 = furthestPointFromOrigin(m3)
Console.log2("r3: ", r3) // 7
