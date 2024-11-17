// T(n) = O(n)
// S(n) = O(1)

let findIntegerAddedToArray_I = (nums1: array<int>, nums2: array<int>) => {
  let rec findMin = (min: int, nums: array<int>, index: int) => {
    switch index === Array.length(nums) {
    | true => min
    | false => {
        let num = switch nums->Array.at(index) {
        | None => Int32.max_int
        | Some(n) => n
        }

        findMin(min < num ? min : num, nums, index + 1)
      }
    }
  }

  let min1 = findMin(Int32.max_int, nums1, 0)
  let min2 = findMin(Int32.max_int, nums2, 0)
  min2 - min1
}

let n1 = [2, 6, 4]
let n11 = [9, 7, 5]
let r1 = findIntegerAddedToArray_I(n1, n11)
Console.log2("r1: ", r1) // 3

let n2 = [10]
let n22 = [5]
let r2 = findIntegerAddedToArray_I(n2, n22)
Console.log2("r2: ", r2) // -5

let n3 = [1, 1, 1, 1]
let n33 = [1, 1, 1, 1]
let r3 = findIntegerAddedToArray_I(n3, n33)
Console.log2("r3: ", r3) // 0
