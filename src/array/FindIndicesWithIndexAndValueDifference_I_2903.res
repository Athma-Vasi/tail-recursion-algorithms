// T(n) = O(n^2)
// S(n) = O(1)

let findIndicesWithIndexAndValueDifference_I = (
  nums: array<int>,
  indexDifference: int,
  valueDifference: int,
) => {
  let length = Array.length(nums)

  let rec outerLoop = (answer: array<int>, outerIndex: int) => {
    switch outerIndex === length {
    | true => answer
    | false => {
        let outerNum = switch nums->Array.at(outerIndex) {
        | None => -1
        | Some(n) => n
        }

        let rec innerLoop = (answer_: array<int>, innerIndex: int) => {
          switch innerIndex >= length {
          | true => answer_
          | false => {
              let innerNum = switch nums->Array.at(innerIndex) {
              | None => -1
              | Some(n) => n
              }

              switch outerNum - innerNum >= valueDifference {
              | true =>
                innerLoop(
                  Array.length(answer_) === 0
                    ? answer_->Array.concat([outerIndex, innerIndex])
                    : answer_,
                  innerIndex + 1,
                )
              | false => innerLoop(answer_, innerIndex + 1)
              }
            }
          }
        }

        let answer_ = innerLoop(answer, outerIndex + indexDifference)
        outerLoop(answer_, outerIndex + 1)
      }
    }
  }

  let answer = outerLoop([], 0)
  switch Array.length(answer) === 0 {
  | true => [-1, -1]
  | false => answer
  }
}

let n1 = [5, 1, 4, 1]
let i1 = 2
let v1 = 4
let r1 = findIndicesWithIndexAndValueDifference_I(n1, i1, v1)
Console.log2("[5,1,4,1], 2, 4", r1) // [0, 3]

let n2 = [2, 1]
let i2 = 0
let v2 = 0
let r2 = findIndicesWithIndexAndValueDifference_I(n2, i2, v2)
Console.log2("[2,1], 0, 0", r2) // [0, 0]

let n3 = [1, 2, 3]
let i3 = 2
let v3 = 4
let r3 = findIndicesWithIndexAndValueDifference_I(n3, i3, v3)
Console.log2("[1,2,3], 2, 4", r3) // []
