// T(n) = O(nlogn)
// S(n) = O(1)

let countPairsWhoseSumIsLessThanTarget = (nums: array<int>, target: int) => {
  let sorted = nums->Array.toSorted((n1, n2) => Int.compare(n1, n2))

  let rec countPairs = (count: int, leftIndex: int, rightIndex: int) => {
    switch leftIndex >= rightIndex {
    | true => count
    | false => {
        let leftNum = switch sorted->Array.at(leftIndex) {
        | None => -51
        | Some(n) => n
        }
        let rightNum = switch sorted->Array.at(rightIndex) {
        | None => 51
        | Some(n) => n
        }
        let sum = leftNum + rightNum

        switch sum < target {
        | true => countPairs(count + rightIndex - leftIndex, leftIndex + 1, rightIndex)
        | false => countPairs(count, leftIndex, rightIndex - 1)
        }
      }
    }
  }

  countPairs(0, 0, Array.length(sorted) - 1)
}

let n1 = [-1, 1, 2, 3, 1]
let t1 = 2
let r1 = countPairsWhoseSumIsLessThanTarget(n1, t1)
Console.log2("r1: ", r1) // 3

let n2 = [-6, 2, 5, -2, -7, -1, 3]
let t2 = -2
let r2 = countPairsWhoseSumIsLessThanTarget(n2, t2)
Console.log2("r2: ", r2) // 10
