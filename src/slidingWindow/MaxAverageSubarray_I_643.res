let maxAverageSubarray_I = (nums: array<int>, k: int) => {
  let rec expandWindow = (window: array<int>, index: int) => {
    switch index === k {
    | true => window
    | false => {
        let num = switch nums->Array.at(index) {
        | None => Int32.min_int
        | Some(n) => n
        }

        expandWindow(window->Array.concat([num]), index + 1)
      }
    }
  }

  let initialWindow = expandWindow([], 0)
  let sum = initialWindow->Array.reduce(0, (total, num) => total + num)
  let average = sum / k

  let rec slideWindow = (~maxAverage: int, ~sum: int, leftIndex: int, rightIndex: int) => {
    switch rightIndex === Array.length(nums) {
    | true => maxAverage
    | false => {
        let leftExcludedNum = switch nums->Array.at(leftIndex) {
        | None => Int32.min_int
        | Some(n) => n
        }

        let rightIncludedNum = switch nums->Array.at(rightIndex + 1) {
        | None => Int32.max_int
        | Some(n) => n
        }

        let newSum = sum - leftExcludedNum + rightIncludedNum
        let newAverage = newSum / k

        slideWindow(
          ~maxAverage=maxAverage > newAverage ? maxAverage : newAverage,
          ~sum=newSum,
          leftIndex + 1,
          rightIndex + 1,
        )
      }
    }
  }

  Array.length(nums) <= k ? sum / k : slideWindow(~maxAverage=average, ~sum, 0, k - 1)
}

let n1 = [1, 12, -5, -6, 50, 3]
let k1 = 4
let r1 = maxAverageSubarray_I(n1, k1)
Console.log2("r1: ", r1)

let n2 = [5]
let k2 = 1
let r2 = maxAverageSubarray_I(n2, k2)
Console.log2("r2: ", r2)
