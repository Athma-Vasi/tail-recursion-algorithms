// T(n) = O(n)
// S(n) = O(1)

let minimumOperationsToCollectElements = (nums: array<int>, k: int) => {
  let rec findMinOps = (ops: int, tracker: int, sliced: array<int>) => {
    let length = Array.length(sliced)

    switch tracker === 0 || length === 0 {
    | true => ops
    | false => {
        let num = switch sliced->Array.at(-1) {
        | None => 0
        | Some(n) => n
        }

        findMinOps(
          ops + 1,
          num <= k ? tracker - 1 : tracker,
          sliced->Array.slice(~start=0, ~end=length - 1),
        )
      }
    }
  }

  findMinOps(0, k, nums->Array.map(n => n))
}

let n1 = [3, 1, 5, 4, 2]
let k1 = 2
let r1 = minimumOperationsToCollectElements(n1, k1)
Console.log2("r1: ", r1) // 4

let n2 = [3, 1, 5, 4, 2]
let k2 = 5
let r2 = minimumOperationsToCollectElements(n2, k2)
Console.log2("r2: ", r2) // 5

let n3 = [3, 2, 5, 3, 1]
let k3 = 3
let r3 = minimumOperationsToCollectElements(n3, k3)
Console.log2("r3: ", r3) // 4
