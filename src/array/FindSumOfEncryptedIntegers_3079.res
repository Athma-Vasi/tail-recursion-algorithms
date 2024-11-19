// T(n) = O(n)
// S(n) = O(n)

let findSumOfEncryptedIntegers = (nums: array<int>) => {
  let rec findSum = (sum: int, index: int) => {
    switch index === Array.length(nums) {
    | true => sum
    | false => {
        let num = switch nums->Array.at(index) {
        | None => 0
        | Some(n) => n
        }

        let rec findMax = (max: int, numStr: string, idx: int) => {
          switch idx === String.length(numStr) {
          | true => max
          | false => {
              let numChar = numStr->String.charAt(idx)
              let number = switch Int.fromString(numChar) {
              | None => 0
              | Some(n) => n
              }

              findMax(max > number ? max : number, numStr, idx + 1)
            }
          }
        }

        let numStr = Int.toString(num)
        let max = findMax(Int32.min_int, numStr, 0)

        let rec replaceWithMax = (replaced: string, numStr: string, max: int, idx: int) => {
          switch idx === String.length(numStr) {
          | true => replaced
          | false => {
              let numChar = numStr->String.charAt(idx)
              let number = switch Int.fromString(numChar) {
              | None => 0
              | Some(n) => n
              }
              let larger = number > max ? number : max

              replaceWithMax(replaced->String.concat(Int.toString(larger)), numStr, max, idx + 1)
            }
          }
        }

        let replaced = replaceWithMax(String.make(), numStr, max, 0)
        let replacedNum = switch Int.fromString(replaced) {
        | None => 0
        | Some(n) => n
        }

        findSum(sum + replacedNum, index + 1)
      }
    }
  }

  findSum(0, 0)
}

let n1 = [1, 2, 3]
let r1 = findSumOfEncryptedIntegers(n1)
Console.log2("r1: ", r1) // 6

let n2 = [10, 21, 31]
let r2 = findSumOfEncryptedIntegers(n2)
Console.log2("r2: ", r2) // 66
