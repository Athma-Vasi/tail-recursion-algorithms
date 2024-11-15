// T(n) = O(n)
// S(n) = O(1)

let findMinOpsToMakeAllElementsDivisibleByThree = (nums: array<int>) => {
  let rec loop = (operations: int, index: int) => {
    switch index === Array.length(nums) {
    | true => operations
    | false => {
        let num = switch nums->Array.at(index) {
        | None => 0
        | Some(n) => n
        }

        Float.mod(Int.toFloat(num), 3.0) === 0.0
          ? loop(operations, index + 1)
          : loop(operations + 1, index + 1)
      }
    }
  }

  loop(0, 0)
}

let n1 = [1, 2, 3, 4]
let r1 = findMinOpsToMakeAllElementsDivisibleByThree(n1)
Console.log2("r1: ", r1) // 3

let n2 = [3, 6, 9]
let r2 = findMinOpsToMakeAllElementsDivisibleByThree(n2)
Console.log2("r2: ", r2) // 0
