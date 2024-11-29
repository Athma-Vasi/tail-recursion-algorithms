// T(n) = O(n)
// S(n) = O(1)

let divisibleAndNonDivisibleSumsDifference = (num: int, divisor: int) => {
  let rec loop = (num1: float, num2: float, reduced: float) => {
    switch reduced === 0.0 {
    | true => (num1, num2)
    | false => {
        let isDivisible = Float.mod(reduced, Int.toFloat(divisor)) === 0.0

        isDivisible
          ? loop(num1, num2 +. reduced, reduced -. 1.0)
          : loop(num1 +. reduced, num2, reduced -. 1.0)
      }
    }
  }

  let (num1, num2) = loop(0.0, 0.0, Int.toFloat(num))
  Float.toInt(num1 -. num2)
}

let n1 = 10
let d1 = 3
let r1 = divisibleAndNonDivisibleSumsDifference(n1, d1)
Console.log2("r1: ", r1) // 19

let n2 = 5
let d2 = 6
let r2 = divisibleAndNonDivisibleSumsDifference(n2, d2)
Console.log2("r2: ", r2) // 15

let n3 = 5
let d3 = 1
let r3 = divisibleAndNonDivisibleSumsDifference(n3, d3)
Console.log2("r3: ", r3) // -15
