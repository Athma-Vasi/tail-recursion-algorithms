// T(n) = O(k * log(n))
// S(n) = O(n)

let finalArrayStateAfterKMultiplicationOps_I = (nums: array<int>, k: int, multiplier: int) => {
  let rec loop = (result: array<int>, operations: int) => {
    switch operations > k {
    | true => result
    | false => {
        let min = switch result->Array.toSorted((a, b) => float(a - b))->Array.at(0) {
        | None => -1
        | Some(n) => n
        }
        let index = result->Array.findIndex(num => num === min)
        result->Array.set(index, min * multiplier)

        loop(result, operations + 1)
      }
    }
  }

  loop(Array.copy(nums), 1)
}

let n1 = [2, 1, 3, 5, 6]
let k1 = 5
let m1 = 2
let r1 = finalArrayStateAfterKMultiplicationOps_I(n1, k1, m1)
Console.log2("r1: ", r1) // [8, 4, 6, 5, 6]

let n2 = [1, 2]
let k2 = 3
let m2 = 4
let r2 = finalArrayStateAfterKMultiplicationOps_I(n2, k2, m2)
Console.log2("r2: ", r2) // [16, 8]
