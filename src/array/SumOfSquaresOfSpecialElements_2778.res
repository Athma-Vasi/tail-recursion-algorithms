// T(n) = O(n)
// S(n) = O(1)
// solution assumes that the array is 0-indexed

let sumOfSquaresOfSpecialElements = (nums: array<int>) => {
  let rec sumSpecialSquares = (sum: int, index: int) => {
    switch index === Array.length(nums) {
    | true => sum
    | false => {
        let num = nums->Array.at(index)->Option.mapOr(0, n => n)
        let remainder = Float.mod(Int.toFloat(num), Int.toFloat(index))

        sumSpecialSquares(remainder === 0.0 ? sum + num * num : sum, index + 1)
      }
    }
  }

  sumSpecialSquares(0, 0)
}

let n1 = [1, 2, 3, 4]
let r1 = sumOfSquaresOfSpecialElements(n1)
Console.log2("r1: ", r1)

let n2 = [2, 7, 1, 19, 18, 3]
let r2 = sumOfSquaresOfSpecialElements(n2)
Console.log2("r2: ", r2)
