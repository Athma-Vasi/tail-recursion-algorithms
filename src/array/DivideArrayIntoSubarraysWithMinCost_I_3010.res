// T(n) = O(n*log(n))
// S(n) = O(n)

let divideArrayIntoSubarraysWithMinCost_I = (nums: array<int>) => {
  let cost = switch nums->Array.at(0) {
  | None => 0
  | Some(n) => n
  }

  let sorted =
    nums
    ->Array.slice(~start=1, ~end=Array.length(nums))
    ->Array.toSorted((a, b) => Int.compare(a, b))

  let firstMin = switch sorted->Array.at(0) {
  | None => 0
  | Some(n) => n
  }
  let secondMin = switch sorted->Array.at(1) {
  | None => 0
  | Some(n) => n
  }

  cost + firstMin + secondMin
}

let n1 = [1, 2, 3, 12]
let r1 = divideArrayIntoSubarraysWithMinCost_I(n1)
Console.log2("r1: ", r1) // 6

let n2 = [5, 4, 3]
let r2 = divideArrayIntoSubarraysWithMinCost_I(n2)
Console.log2("r2: ", r2) // 12

let n3 = [10, 3, 1, 1]
let r3 = divideArrayIntoSubarraysWithMinCost_I(n3)
Console.log2("r3: ", r3) // 12
