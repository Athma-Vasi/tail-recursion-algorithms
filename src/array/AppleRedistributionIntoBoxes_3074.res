// T(n) = O(log(n))
// S(n) = O(n)

let appleRedistributionIntoBoxes = (apples: array<int>, capacities: array<int>) => {
  let total = apples->Array.reduce(0, (total, apple) => total + apple)
  let sortedCapacitiesDesc = capacities->Array.toSorted((a, b) => float(b - a))

  let rec redistribute = (boxes: int, capacities: array<int>, total: int) => {
    let capLength = Array.length(capacities)
    switch total < 1 || capLength === 0 {
    | true => boxes
    | false => {
        let box = switch capacities->Array.at(0) {
        | None => 0
        | Some(b) => b
        }

        switch total >= box {
        | true =>
          redistribute(boxes + 1, capacities->Array.slice(~start=1, ~end=capLength), total - box)
        | false => {
            let newCapacity = box - total
            capacities->Array.set(0, newCapacity)

            redistribute(boxes + 1, capacities, 0)
          }
        }
      }
    }
  }

  redistribute(0, sortedCapacitiesDesc, total)
}

let a1 = [1, 3, 2]
let c1 = [4, 3, 1, 5, 2]
let r1 = appleRedistributionIntoBoxes(a1, c1)
Console.log2("r1: ", r1) // 2

let a2 = [5, 5, 5]
let c2 = [2, 4, 2, 7]
let r2 = appleRedistributionIntoBoxes(a2, c2)
Console.log2("r2: ", r2) // 4
