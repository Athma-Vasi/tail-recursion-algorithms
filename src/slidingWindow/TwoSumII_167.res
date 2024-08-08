// T(n) = O(n)
// S(n) = O(1)

let twoSumII = (nums: array<int>, target: int) => {
  let length = Array.length(nums)

  let rec loop = (leftIndex: int, rightIndex: int) => {
    let leftNum = switch nums->Array.get(leftIndex) {
    | None => -1
    | Some(num) => num
    }
    let rightNum = switch nums->Array.get(rightIndex) {
    | None => -1
    | Some(num) => num
    }
    let sum = leftNum + rightNum

    switch sum === target || leftIndex === rightIndex {
    | true => (leftIndex + 1, rightIndex + 1) // 1-based indexing required by leetcode
    | false =>
      switch sum > target {
      | true => loop(leftIndex, rightIndex - 1)
      | false => loop(leftIndex + 1, rightIndex)
      }
    }
  }

  loop(0, length - 1)
}

let n1 = [2, 7, 11, 15]
let r1 = twoSumII(n1, 9)
Console.log2("r1", r1)
