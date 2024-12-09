// T(n) = O(n)
// S(n) = O(1)

let wateringPlants = (plants: array<int>, capacity: int) => {
  let rec waterPlants = (steps: int, can: int, index: int) => {
    switch index === Array.length(plants) {
    | true => steps
    | false => {
        let req = switch plants->Array.at(index) {
        | None => 0
        | Some(r) => r
        }

        switch can < req || can <= 0 {
        | true => waterPlants(steps + index * 2, capacity, index)
        | false => waterPlants(steps + 1, can - req, index + 1)
        }
      }
    }
  }

  waterPlants(0, capacity, 0)
}

let p1 = [2, 2, 3, 3]
let c1 = 5
let r1 = wateringPlants(p1, c1)
Console.log2("r1: ", r1) // 14

let p2 = [1, 1, 1, 4, 2, 3]
let c2 = 4
let r2 = wateringPlants(p2, c2)
Console.log2("r2: ", r2) // 30
