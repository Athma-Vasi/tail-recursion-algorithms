// T(n) = O(n * m log m) where m <= n
// S(n) = O(n + m)

let slidingWindowMedian = (numbers: array<int>, windowSize: int) => {
  let length = Array.length(numbers)

  let rec loop = (medianArrays: array<float>, leftIndex: int, rightIndex: int) => {
    switch rightIndex === length {
    | true => medianArrays
    | false => {
        let slicedWindow = numbers->Array.slice(~start=leftIndex, ~end=rightIndex + 1)
        slicedWindow->Array.sort((a, b) => float(a - b))

        let isWindowSizeEven = Float.mod(Int.toFloat(windowSize), 2.0) == 0.0

        switch isWindowSizeEven {
        | true => {
            let rightMedianIndex = (Int.toFloat(windowSize) /. 2.0)->Float.toInt
            let rightMedianNumber = switch slicedWindow->Array.get(rightMedianIndex) {
            | None => Int32.min_int
            | Some(num) => num
            }

            let leftMedianIndex = rightMedianIndex - 1
            let leftMedianNumber = switch slicedWindow->Array.get(leftMedianIndex) {
            | None => Int32.min_int
            | Some(num) => num
            }
            let averageMedianNumber = (leftMedianNumber + rightMedianNumber)->Int.toFloat /. 2.0

            loop(medianArrays->Array.concat([averageMedianNumber]), leftIndex + 1, rightIndex + 1)
          }
        | false => {
            let medianIndex = Math.floor(Int.toFloat(windowSize) /. 2.0)->Float.toInt
            let medianNumber = switch slicedWindow->Array.get(medianIndex) {
            | None => Int32.min_int
            | Some(num) => num
            }->Int.toFloat

            loop(medianArrays->Array.concat([medianNumber]), leftIndex + 1, rightIndex + 1)
          }
        }
      }
    }
  }

  loop([], 0, windowSize - 1)
}

let n1 = [1, 3, -1, -3, 5, 3, 6, 7]
let w1 = 3
let r1 = slidingWindowMedian(n1, w1)
Console.log2("r1", r1)

let n2 = [1, 2, 3, 4, 2, 3, 1, 4, 2]
let w2 = 3
let r2 = slidingWindowMedian(n2, w2)
Console.log2("r2", r2)
