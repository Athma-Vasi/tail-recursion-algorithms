// T(n) = O(n^2)
// S(n) = O(1)

let findNumberOfGoodPairs_I = (nums1: array<int>, nums2: array<int>, k: int) => {
  let rec nums1Loop = (count: int, index1: int) => {
    switch index1 === Array.length(nums1) {
    | true => count
    | false => {
        let num1 = switch nums1->Array.at(index1) {
        | None => -1
        | Some(n) => n
        }

        let rec nums2Loop = (tempCount: int, index2: int) => {
          switch index2 === Array.length(nums2) {
          | true => tempCount
          | false => {
              let num2 = switch nums2->Array.at(index2) {
              | None => -1
              | Some(n) => n
              }
              let divisor = num2 * k
              let remainder = Float.mod(Int.toFloat(num1), Int.toFloat(divisor))

              remainder === 0.0
                ? nums2Loop(tempCount + 1, index2 + 1)
                : nums2Loop(tempCount, index2 + 1)
            }
          }
        }

        nums1Loop(nums2Loop(count, 0), index1 + 1)
      }
    }
  }

  nums1Loop(0, 0)
}

let n1 = [1, 3, 4]
let n11 = [1, 3, 4]
let k1 = 1
let r1 = findNumberOfGoodPairs_I(n1, n11, k1)
Console.log2("r1: ", r1) // 5

let n2 = [1, 2, 4, 12]
let n22 = [2, 4]
let k2 = 3
let r2 = findNumberOfGoodPairs_I(n2, n22, k2)
Console.log2("r2: ", r2) // 2
