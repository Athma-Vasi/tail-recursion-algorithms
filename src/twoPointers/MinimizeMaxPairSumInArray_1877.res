// T(n) = O(n * log(n))
// S(n) = O(n)

let minimizeMaxPairSumInArray = (nums: array<int>) => {
  let sorted = nums->Array.toSorted((n1, n2) => Int.compare(n1, n2))

  let rec minimize = (maxPairSum: int, leftIndex: int, rightIndex: int) => {
    switch leftIndex >= rightIndex {
    | true => maxPairSum
    | false => {
        let leftNum = switch sorted->Array.at(leftIndex) {
        | None => 0
        | Some(n) => n
        }
        let rightNum = switch sorted->Array.at(rightIndex) {
        | None => 0
        | Some(n) => n
        }
        let pairSum = leftNum + rightNum

        minimize(pairSum > maxPairSum ? pairSum : maxPairSum, leftIndex + 1, rightIndex - 1)
      }
    }
  }

  minimize(Int32.min_int, 0, Array.length(sorted) - 1)
}

let n1 = [3, 5, 2, 3]
let r1 = minimizeMaxPairSumInArray(n1)
Console.log2("r1: ", r1) // 7

let n2 = [3, 5, 4, 2, 4, 6]
let r2 = minimizeMaxPairSumInArray(n2)
Console.log2("r2: ", r2) // 8
