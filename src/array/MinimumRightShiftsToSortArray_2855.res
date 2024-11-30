// T(n) = O(n^2)
// S(n) = O(n)

let minimumRightShiftsToSortArray = (nums: array<int>) => {
  let length = Array.length(nums)

  let rightShiftElements = (nums: array<int>) => {
    let last = switch nums->Array.at(-1) {
    | None => 0
    | Some(n) => n
    }

    [last]->Array.concat(nums->Array.slice(~start=0, ~end=length - 1))
  }

  let checkIsSorted = (nums: array<int>) => {
    let rec check = (isSorted: bool, prev: int, nums: array<int>, index: int) => {
      switch index === Array.length(nums) || !isSorted {
      | true => isSorted
      | false => {
          let curr = switch nums->Array.at(index) {
          | None => 0
          | Some(n) => n
          }

          check(curr > prev ? isSorted : false, curr, nums, index + 1)
        }
      }
    }

    check(true, Int32.min_int, nums, 0)
  }

  let rec findMinRightShifts = (limit: int, isSorted: bool, shifted: array<int>) => {
    switch limit === length || isSorted {
    | true => limit === length ? -1 : limit
    | false => {
        let rightShifted = rightShiftElements(shifted)
        let isSorted_ = checkIsSorted(rightShifted)

        findMinRightShifts(limit + 1, isSorted_ ? isSorted_ : isSorted, rightShifted)
      }
    }
  }

  let isSorted = checkIsSorted(nums)
  isSorted ? 0 : findMinRightShifts(0, isSorted, nums)
}

let n1 = [3, 4, 5, 1, 2]
let r1 = minimumRightShiftsToSortArray(n1)
Console.log2("r1: ", r1) // 2

let n2 = [1, 3, 5]
let r2 = minimumRightShiftsToSortArray(n2)
Console.log2("r2: ", r2) // 0

let n3 = [2, 1, 4]
let r3 = minimumRightShiftsToSortArray(n3)
Console.log2("r3: ", r3) // -1
