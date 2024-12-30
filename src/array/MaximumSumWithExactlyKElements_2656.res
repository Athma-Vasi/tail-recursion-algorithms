// T(n) = O(n)
// S(n) = O(1)
// @see https://leetcode.com/problems/maximum-sum-with-exactly-k-elements/solutions/3466678/max-element-formula/

let maximumSumWithExactlyKElements = (nums: array<int>, k: int) => {
  let max = nums->Array.reduce(Int32.min_int, (acc, num) => acc > num ? acc : num)
  max * k + k * (k - 1) / 2
}

let n1 = [1, 2, 3, 4, 5]
let k1 = 3
let r1 = maximumSumWithExactlyKElements(n1, k1)
Console.log2("r1: ", r1) // 18

let n2 = [5, 5, 5]
let k2 = 2
let r2 = maximumSumWithExactlyKElements(n2, k2)
Console.log2("r2: ", r2) // 11
