// T(n) = O(n!)
// S(n) = O(n!)

let findTriangularSumOfArray = (nums: array<int>) => {
  let rec findTriangularSum = (sums: array<int>) => {
    switch Array.length(sums) === 1 {
    | true =>
      switch sums->Array.at(0) {
      | None => -1
      | Some(s) => s
      }
    | false => {
        let rec rowSums = (stack: array<int>, index: int) => {
          switch index + 1 >= Array.length(sums) {
          | true => stack
          | false => {
              let curr = switch sums->Array.at(index) {
              | None => 0
              | Some(value) => value
              }
              let next = switch sums->Array.at(index + 1) {
              | None => 0
              | Some(value) => value
              }
              let newValue = Float.mod(Int.toFloat(curr + next), 10.0)

              rowSums(stack->Array.concat([Int.fromFloat(newValue)]), index + 1)
            }
          }
        }

        findTriangularSum(rowSums([], 0))
      }
    }
  }

  findTriangularSum(nums)
}

let n1 = [1, 2, 3, 4, 5]
let r1 = findTriangularSumOfArray(n1)
Console.log2("r1: ", r1) // 8

let n2 = [5]
let r2 = findTriangularSumOfArray(n2)
Console.log2("r2: ", r2)
