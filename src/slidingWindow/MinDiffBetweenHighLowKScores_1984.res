// T(n) = O(nlogn + n) = O(nlogn)
// S(n) = O(n)

let minDiffBetweenHighLowKScores = (nums: array<int>, k: int) => {
  let length = Array.length(nums)
  let sorted = nums->Array.map(num => num)
  sorted->Array.sort((a, b) => float(a - b))

  let findAbsDiff = (leftNum: int, rightNum: int) => {
    let diff = leftNum - rightNum
    diff < 0 ? diff * -1 : diff
  }

  let rec expandWindow = (~minDiff: int, ~index: int, ~leftNum: int) => {
    switch index === k {
    | true => minDiff
    | false => {
        let currentNum = switch nums->Array.at(index) {
        | None => Int32.min_int
        | Some(num) => num
        }
        let absDiff = findAbsDiff(leftNum, currentNum)

        expandWindow(~minDiff=absDiff < minDiff ? absDiff : minDiff, ~index=index + 1, ~leftNum)
      }
    }
  }

  let rec loop = (~minDiff: int, ~leftIndex: int, ~rightIndex: int) => {
    switch rightIndex === length {
    | true => minDiff
    | false => {
        let leftNum = switch sorted->Array.at(leftIndex) {
        | None => Int32.min_int
        | Some(num) => num
        }
        let rightNum = switch sorted->Array.at(rightIndex) {
        | None => Int32.min_int + 1
        | Some(num) => num
        }
        let absDiff = findAbsDiff(leftNum, rightNum)

        loop(
          ~minDiff=absDiff < minDiff ? absDiff : minDiff,
          ~leftIndex=leftIndex + 1,
          ~rightIndex=rightIndex + 1,
        )
      }
    }
  }

  let firstNum = switch sorted->Array.at(0) {
  | None => Int32.min_int
  | Some(num) => num
  }

  let minDiff = loop(
    ~minDiff=expandWindow(~minDiff=Int32.max_int, ~index=1, ~leftNum=firstNum),
    ~leftIndex=1,
    ~rightIndex=k,
  )

  switch minDiff === Int32.max_int {
  | true => 0
  | false => minDiff
  }
}

let n1 = [90]
let k1 = 1
let r1 = minDiffBetweenHighLowKScores(n1, k1)
Console.log2("r1: ", r1)

let n2 = [9, 4, 1, 7]
let k2 = 2
let r2 = minDiffBetweenHighLowKScores(n2, k2)
Console.log2("r2: ", r2)
