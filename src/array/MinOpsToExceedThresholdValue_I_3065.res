// T(n) = O(n)
// S(n) = O(1)

let minOpsToExceedThresholdValue_I = (nums: array<int>, k: int) => {
  let rec loop = (operations: int, index: int) => {
    switch index === Array.length(nums) {
    | true => operations
    | false => {
        let num = switch nums->Array.at(index) {
        | None => 0
        | Some(n) => n
        }

        loop(num >= k ? operations : operations + 1, index + 1)
      }
    }
  }

  loop(0, 0)
}

let n1 = [2, 11, 10, 1, 3]
let k1 = 10
let r1 = minOpsToExceedThresholdValue_I(n1, k1)
Console.log2("r1: ", r1) // 3

let n2 = [1, 1, 2, 4, 9]
let k2 = 1
let r2 = minOpsToExceedThresholdValue_I(n2, k2)
Console.log2("r2: ", r2) // 0

let n3 = [1, 1, 2, 4, 9]
let k3 = 9
let r3 = minOpsToExceedThresholdValue_I(n3, k3)
Console.log2("r3: ", r3) // 4
