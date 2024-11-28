// T(n) = O(n)
// S(n) = O(1)

let findIndicesWithIndexAndValueDifference_II = (
  nums: array<int>,
  indexDifference: int,
  valueDifference: int,
) => {
  let rec slideWindow = (
    ~answer: array<int>,
    ~minIdx: int,
    ~maxIdx: int,
    ~leftIdx: int,
    ~rightIdx: int,
  ) => {
    switch rightIdx === Array.length(nums) || Array.length(answer) !== 0 {
    | true => answer
    | false => {
        let leftNum = switch nums->Array.at(leftIdx) {
        | None => -1
        | Some(n) => n
        }
        let minNum = switch nums->Array.at(minIdx) {
        | None => -1
        | Some(n) => n
        }
        let maxNum = switch nums->Array.at(maxIdx) {
        | None => -1
        | Some(n) => n
        }
        let rightNum = switch nums->Array.at(rightIdx) {
        | None => -1
        | Some(n) => n
        }

        let minDiff = minNum - rightNum
        let absMinDiff = minDiff < 0 ? minDiff * -1 : minDiff
        let maxDiff = maxNum - rightNum
        let absMaxDiff = maxDiff < 0 ? maxDiff * -1 : maxDiff

        let newAnswer =
          Array.length(answer) === 0
            ? answer->Array.concat(
                absMinDiff >= valueDifference
                  ? [minIdx, rightIdx]
                  : absMaxDiff >= valueDifference
                  ? [maxIdx, rightIdx]
                  : [],
              )
            : answer

        slideWindow(
          ~answer=newAnswer,
          ~leftIdx=leftIdx + 1,
          ~maxIdx=maxNum > leftNum ? maxIdx : leftIdx,
          ~minIdx=minNum < leftNum ? minIdx : leftIdx,
          ~rightIdx=rightIdx + 1,
        )
      }
    }
  }

  let answer = slideWindow(~answer=[], ~leftIdx=0, ~maxIdx=0, ~minIdx=0, ~rightIdx=indexDifference)
  switch Array.length(answer) === 0 {
  | true => [-1, -1]
  | false => answer
  }
}

let n1 = [5, 1, 4, 1]
let i1 = 2
let v1 = 4
let r1 = findIndicesWithIndexAndValueDifference_II(n1, i1, v1)
Console.log2("[5,1,4,1], 2, 4", r1) // [0, 3]

let n2 = [2, 1]
let i2 = 0
let v2 = 0
let r2 = findIndicesWithIndexAndValueDifference_II(n2, i2, v2)
Console.log2("[2,1], 0, 0", r2) // [0, 0]

let n3 = [1, 2, 3]
let i3 = 2
let v3 = 4
let r3 = findIndicesWithIndexAndValueDifference_II(n3, i3, v3)
Console.log2("[1,2,3], 2, 4", r3) // []
