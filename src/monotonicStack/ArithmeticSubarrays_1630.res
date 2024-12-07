// T(n) = O(m * n * log(n)) where m is the length of starts and ends
// S(n) = O(m + n)

let arithmeticSubarrays = (numbers: array<int>, starts: array<int>, ends: array<int>) => {
  let checkIfArithmeticSequence = (nums: array<int>) => {
    let length = Array.length(nums)

    let rec checkIfMonotonicallyIncreasing = (diffs: Set.t<int>, prev: int, index: int) => {
      switch index === length {
      | true => Set.size(diffs) === 1
      | false => {
          let num = switch nums->Array.at(index) {
          | None => 0
          | Some(n) => n
          }
          diffs->Set.add(prev - num)

          checkIfMonotonicallyIncreasing(diffs, num, index + 1)
        }
      }
    }

    let first = switch nums->Array.at(0) {
    | None => 0
    | Some(n) => n
    }

    checkIfMonotonicallyIncreasing(Set.make(), first, 1)
  }

  let rec determineArithmeticSubarrays = (answer: array<bool>, index: int) => {
    switch index === Array.length(starts) || index === Array.length(ends) {
    | true => answer
    | false => {
        let start = switch starts->Array.at(index) {
        | None => -1
        | Some(n) => n
        }
        let end = switch ends->Array.at(index) {
        | None => -1
        | Some(n) => n + 1
        }

        let sortedAsc =
          numbers
          ->Array.slice(~start, ~end)
          ->Array.toSorted((n1, n2) => Int.compare(n1, n2))
        let isArithmeticSequence = checkIfArithmeticSequence(sortedAsc)

        determineArithmeticSubarrays(answer->Array.concat([isArithmeticSequence]), index + 1)
      }
    }
  }

  determineArithmeticSubarrays([], 0)
}

let n1 = [4, 6, 5, 9, 3, 7]
let s1 = [0, 0, 2]
let e1 = [2, 3, 5]
let r1 = arithmeticSubarrays(n1, s1, e1)
Console.log2("r1: ", r1) // [true, false, true]

let n2 = [-12, -9, -3, -12, -6, 15, 20, -25, -20, -15, -10]
let s2 = [0, 1, 6, 4, 8, 7]
let e2 = [4, 4, 9, 7, 9, 10]
let r2 = arithmeticSubarrays(n2, s2, e2)
Console.log2("r2: ", r2) // [false,true,false,false,true,true]
