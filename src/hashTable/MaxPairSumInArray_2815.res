// T(n) = O(n)
// S(n) = O(n)

let maxPairSumInArray = (nums: array<int>) => {
  let rec getMaxDigit = (maxDigit: float, num: float) => {
    switch num <= 0.0 {
    | true => maxDigit
    | false => {
        let mod = Float.mod(num, 10.0)
        let reduced = (num -. mod) /. 10.0

        getMaxDigit(maxDigit > mod ? maxDigit : mod, reduced)
      }
    }
  }

  let rec findMaxPairSum = (max: int, maxTable: Map.t<float, int>, index: int) => {
    switch index === Array.length(nums) {
    | true => max
    | false => {
        let num = switch nums->Array.at(index) {
        | None => 0
        | Some(n) => n
        }
        let maxDigit = getMaxDigit(Int.toFloat(Int32.min_int), Int.toFloat(num))
        let pair = switch maxTable->Map.get(maxDigit) {
        | None => Int32.min_int
        | Some(n) => n
        }

        switch pair === Int32.min_int {
        | true => maxTable->Map.set(maxDigit, num)
        | false => maxTable->Map.set(maxDigit, num > pair ? num : pair)
        }

        findMaxPairSum(max > num + pair ? max : num + pair, maxTable, index + 1)
      }
    }
  }

  findMaxPairSum(-1, Map.make(), 0)
}

let n1 = [112, 131, 411]
let r1 = maxPairSumInArray(n1)
Console.log2("r1: ", r1) // -1

let n2 = [2536, 1613, 3366, 162]
let r2 = maxPairSumInArray(n2)
Console.log2("r2: ", r2) // 5902

let n3 = [51, 71, 17, 24, 42]
let r3 = maxPairSumInArray(n3)
Console.log2("r3: ", r3) // 88
