// T(n) = O(n)
// S(n) = O(n)

let countNumberOfDistinctIntegersAfterReverseOps = (nums: array<int>) => {
  let reverseDigits = (num: int) => {
    let rec reverse = (reversed: string, numStr: string, index: int) => {
      index < 0
        ? switch Int.fromString(reversed) {
          | None => 0
          | Some(s) => s
          }
        : reverse(reversed->String.concat(numStr->String.charAt(index)), numStr, index - 1)
    }

    let numStr = Int.toString(num)
    let length = String.length(numStr)
    reverse(String.make(), Int.toString(num), length - 1)
  }

  let rec reverseOperation = (distincts: Set.t<int>, index: int) => {
    switch index === Array.length(nums) {
    | true =>
      nums
      ->Array.reduce(distincts, (set, num) => {
        set->Set.add(num)
        set
      })
      ->Set.size
    | false => {
        let num = switch nums->Array.at(index) {
        | None => 0
        | Some(n) => n
        }
        distincts->Set.add(reverseDigits(num))

        reverseOperation(distincts, index + 1)
      }
    }
  }

  reverseOperation(Set.make(), 0)
}

let n1 = [1, 13, 10, 12, 31]
let r1 = countNumberOfDistinctIntegersAfterReverseOps(n1)
Console.log2("r1: ", r1) // 6

let n2 = [2, 2, 2]
let r2 = countNumberOfDistinctIntegersAfterReverseOps(n2)
Console.log2("r2: ", r2) // 1
