// T(n) = O(n ^ 2)
// S(n) = O(n)

let minElementAfterReplacementWithDigitSum = (nums: array<int>) => {
  let rec outerLoop = (result: array<int>, index: int) => {
    switch index === Array.length(nums) {
    | true => result
    | false => {
        let num = switch nums->Array.at(index) {
        | None => -1
        | Some(n) => n
        }
        let numStr = Int.toString(num)

        let rec innerLoop = (sum: int, strIndex: int) => {
          switch strIndex === String.length(numStr) {
          | true => sum
          | false => {
              let char = numStr->String.charAt(strIndex)
              let numChar = switch Int.fromString(char) {
              | None => -1
              | Some(n) => n
              }

              innerLoop(sum + numChar, strIndex + 1)
            }
          }
        }

        let sum = innerLoop(0, 0)
        outerLoop(result->Array.concat([sum]), index + 1)
      }
    }
  }

  switch outerLoop([], 0)->Array.toSorted((a, b) => float(a - b))->Array.at(0) {
  | None => -1
  | Some(arr) => arr
  }
}

let n1 = [10, 12, 13, 14]
let r1 = minElementAfterReplacementWithDigitSum(n1)
Console.log2("r1: ", r1) // 1

let n2 = [1, 2, 3, 4]
let r2 = minElementAfterReplacementWithDigitSum(n2)
Console.log2("r2: ", r2) // 1

let n3 = [999, 19, 199]
let r3 = minElementAfterReplacementWithDigitSum(n3)
Console.log2("r3: ", r3) // 10
