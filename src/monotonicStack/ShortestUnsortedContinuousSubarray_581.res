// T(n) = O(n)
// S(n) = O(n)

let shortestUnsortedContinousSubarray = (nums: array<int>) => {
  let length = Array.length(nums)

  // returns the largest index whose number is smaller than previous
  let rec increasingLoop = (maximumIndex: int, monoIncrStack: array<int>, index: int) => {
    switch index === length {
    | true => maximumIndex
    | false => {
        let stackLength = Array.length(monoIncrStack)
        let currentNum = switch nums->Array.get(index) {
        | None => Int32.min_int
        | Some(num) => num
        }

        switch stackLength < 1 {
        | true => increasingLoop(maximumIndex, monoIncrStack->Array.concat([index]), index + 1)
        | false => {
            let prevMaximumIndex = switch monoIncrStack->Array.at(-1) {
            | None => Int32.min_int
            | Some(num) => num
            }
            let prevMaximum = switch nums->Array.get(prevMaximumIndex) {
            | None => Int32.min_int
            | Some(num) => num
            }

            switch currentNum < prevMaximum {
            | true =>
              increasingLoop(
                index, // new maximum index
                monoIncrStack->Array.slice(~start=0, ~end=stackLength - 1),
                index,
              )
            | false => increasingLoop(maximumIndex, monoIncrStack->Array.concat([index]), index + 1)
            }
          }
        }
      }
    }
  }

  // returns the smallest index whose number is greater than previous
  let rec decreasingLoop = (minimumIndex: int, monoDecrStack: array<int>, index: int) => {
    switch index === 0 {
    | true => minimumIndex
    | false => {
        let stackLength = Array.length(monoDecrStack)
        let currentNum = switch nums->Array.get(index) {
        | None => Int32.min_int
        | Some(num) => num
        }

        switch stackLength < 1 {
        | true => decreasingLoop(minimumIndex, monoDecrStack->Array.concat([index]), index - 1)
        | false => {
            let prevMinimumIndex = switch monoDecrStack->Array.at(-1) {
            | None => Int32.min_int
            | Some(num) => num
            }
            let prevMinimum = switch nums->Array.get(prevMinimumIndex) {
            | None => Int32.min_int
            | Some(num) => num
            }

            switch currentNum > prevMinimum {
            | true =>
              decreasingLoop(
                index, // new minimum index
                monoDecrStack->Array.slice(~start=0, ~end=stackLength - 1),
                index,
              )
            | false => decreasingLoop(minimumIndex, monoDecrStack->Array.concat([index]), index - 1)
            }
          }
        }
      }
    }
  }

  let maximumIndex = increasingLoop(-1, [], 0)
  let minimumIndex = decreasingLoop(-1, [], length - 1)

  switch minimumIndex < 0 || maximumIndex < 0 {
  | true => 0
  | false => maximumIndex - minimumIndex + 1
  }
}

let n1 = [2, 6, 4, 8, 10, 9, 15]
let r1 = shortestUnsortedContinousSubarray(n1)
Console.log2("r1: ", r1)

let n2 = [1, 2, 3, 4]
let r2 = shortestUnsortedContinousSubarray(n2)
Console.log2("r2: ", r2)

let n3 = [1]
let r3 = shortestUnsortedContinousSubarray(n3)
Console.log2("r3: ", r3)
