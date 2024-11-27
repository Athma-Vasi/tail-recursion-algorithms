// T(n) = O(n^2)
// S(n) = O(n)
// INCORRECT

let subarraysDistinctElementSumOfSquares_I = (nums: array<int>) => {
  let length = Array.length(nums)

  let rec outerLoop = (sum: int, outerIndex: int) => {
    switch outerIndex === length {
    | true => sum
    | false => {
        let rec innerLoop = (innerSum: int, subSet: Set.t<int>, innerIndex: int) => {
          switch innerIndex === length {
          | true => innerSum
          | false => {
              let num = switch nums->Array.at(innerIndex) {
              | None => 0
              | Some(n) => n
              }
              subSet->Set.add(num)

              innerLoop(innerSum + Set.size(subSet) * Set.size(subSet), subSet, innerIndex + 1)
            }
          }
        }

        let innerSum = innerLoop(sum, Set.make(), outerIndex)
        outerLoop(sum + innerSum, outerIndex + 1)
      }
    }
  }

  outerLoop(0, 0)
}

let n1 = [1, 2, 1]
let r1 = subarraysDistinctElementSumOfSquares_I(n1)
Console.log2("[1,2,1]", r1) // 15

let n2 = [1, 1]
let r2 = subarraysDistinctElementSumOfSquares_I(n2)
Console.log2("[1,1]", r2) // 3
