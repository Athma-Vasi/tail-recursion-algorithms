// T(n) = O(log(x))
// S(n) = O(1)

let harshadNumber = (x: int) => {
  let rec loop = (sum: int, temp: int) => {
    switch temp < 1 {
    | true => sum
    | false => {
        let remainder = Float.mod(Int.toFloat(temp), 10.0)

        loop(sum + Float.toInt(remainder), temp / 10)
      }
    }
  }

  let sum = loop(0, x)
  switch Float.mod(Int.toFloat(x), Int.toFloat(sum)) === 0.0 {
  | true => sum
  | false => -1
  }
}

let x1 = 18
let r1 = harshadNumber(x1)
Console.log2("r1: ", r1) // 9

let x2 = 23
let r2 = harshadNumber(x2)
Console.log2("r2: ", r2) // -1
