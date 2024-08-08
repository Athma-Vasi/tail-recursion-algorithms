// T(n) = O(n^2)
// S(n) = O(n)

let nextGreaterElement = (nums1: array<int>, nums2: array<int>): array<int> => {
  let nums1Length = Array.length(nums1)
  let nums2Length = Array.length(nums2)

  let rec nums1Loop = (accumulator: array<int>, nums1Index: int): array<int> => {
    let currentNum1 = switch nums1->Array.get(nums1Index) {
    | None => Int32.min_int
    | Some(num) => num
    }
    let nums2Index = nums2->Array.findIndex(num2 => num2 === currentNum1)

    let rec nums2Loop = (greaterNum: int, nums2Index: int): int => {
      switch nums2Index === nums2Length - 1 {
      | true => greaterNum
      | false => {
          let currentNum2 = switch nums2->Array.get(nums2Index) {
          | None => Int32.min_int
          | Some(num) => num
          }

          switch currentNum2 > currentNum1 {
          | true => currentNum2
          | false => nums2Loop(greaterNum, nums2Index + 1)
          }
        }
      }
    }

    let greaterNum = nums2Loop(-1, nums2Index)
    switch nums1Index === nums1Length {
    | true => accumulator
    | false => nums1Loop(accumulator->Array.concat([greaterNum]), nums1Index + 1)
    }
  }

  nums1Loop([], 0)
}

let nums1 = [4, 1, 2]
let nums2 = [1, 3, 4, 2]
let result1 = nextGreaterElement(nums1, nums2)
Console.log2("result1", result1)
