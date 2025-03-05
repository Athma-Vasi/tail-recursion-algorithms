// T(n) = O(n * log(n))
// S(n) = O(1)

let largestPositiveIntegerThatExistsWithItsNegative = (nums: array<int>) => {
  let sorted = nums->Array.toSorted((n1, n2) => Int.compare(n1, n2))

  let rec findLargest = (largest: int, leftIndex: int, rightIndex: int) => {
    switch largest !== Int32.min_int || leftIndex >= rightIndex {
    | true => largest
    | false => {
        let leftNum = sorted->Array.at(leftIndex)->Option.mapOr(Int32.min_int, n => n)
        let rightNum = sorted->Array.at(rightIndex)->Option.mapOr(Int32.max_int, n => n)

        switch leftNum + rightNum === 0 {
        | true => findLargest(rightNum, leftIndex + 1, rightIndex - 1)
        | false =>
          switch leftNum + rightNum < 0 {
          | true => findLargest(largest, leftIndex + 1, rightIndex)
          | false => findLargest(largest, leftIndex, rightIndex - 1)
          }
        }
      }
    }
  }

  let largest = findLargest(Int32.min_int, 0, Array.length(nums) - 1)
  largest === Int32.min_int ? -1 : largest
}

let n1 = [-1, 2, -3, 3]
let r1 = largestPositiveIntegerThatExistsWithItsNegative(n1)
Console.log2("r1: ", r1) // 3

let n2 = [-1, 10, 6, 7, -7, 1]
let r2 = largestPositiveIntegerThatExistsWithItsNegative(n2)
Console.log2("r2: ", r2) // 7

let n3 = [-10, 8, 6, 7, -2, -3]
let r3 = largestPositiveIntegerThatExistsWithItsNegative(n3)
Console.log2("r3: ", r3) // -1
