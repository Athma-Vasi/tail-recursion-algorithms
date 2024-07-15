// T(n) = O(n)
// S(n) = O(1)

let minSizeSubArraySum = (nums: array<int>, target: int) => {
  let length = Array.length(nums)

  let rec loop = (~accumulator: int, ~minSize: int, ~lowIndex: int, ~highIndex: int) => {
    switch highIndex === length {
    | true => minSize === Int32.max_int ? 0 : minSize
    | false =>
      switch accumulator >= target {
      | true => {
          let currentMinSize = highIndex - lowIndex + 1
          let newMinSize = Math.min(Int.toFloat(minSize), Int.toFloat(currentMinSize))->Float.toInt

          let leftNum = switch nums->Array.get(lowIndex) {
          | None => Int32.min_int
          | Some(num) => num
          }

          loop(
            ~accumulator=accumulator - leftNum,
            ~minSize=newMinSize,
            ~lowIndex=lowIndex + 1,
            ~highIndex,
          )
        }
      | false => {
          let newHighIndex = highIndex + 1
          let rightNum = switch nums->Array.get(newHighIndex) {
          | None => Int32.min_int
          | Some(num) => num
          }

          loop(~accumulator=accumulator + rightNum, ~minSize, ~lowIndex, ~highIndex=newHighIndex)
        }
      }
    }
  }

  switch length < 1 {
  | true => 0
  | false => {
      let firstNum = switch nums->Array.get(0) {
      | None => Int32.min_int
      | Some(num) => num
      }
      let secondNum = switch nums->Array.get(1) {
      | None => Int32.min_int
      | Some(num) => num
      }
      let accumulator = firstNum + secondNum

      loop(~accumulator, ~minSize=Int32.max_int, ~lowIndex=0, ~highIndex=1)
    }
  }
}

let n1 = [2, 3, 1, 2, 4, 3]
let r1 = minSizeSubArraySum(n1, 7)
Console.log2("[2,3,1,2,4,3] 7", r1)

let n2 = [1, 4, 4]
let r2 = minSizeSubArraySum(n2, 4)
Console.log2("[1,4,4] 4", r2)

let n3 = [1, 1, 1, 1, 1, 1, 1]
let r3 = minSizeSubArraySum(n3, 11)
Console.log2("[1,1,1,1,1,1,1] 11", r3)
