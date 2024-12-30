// T(n) = O(n)
// S(n) = O(1)

let sumMultiples = (n: int) => {
  let rec sum = (answer: int, counter: int) => {
    switch counter > n {
    | true => answer
    | false => {
        let isDivisibleBy3 = Float.mod(Int.toFloat(counter), 3.0) === 0.0
        let isDivisibleBy5 = Float.mod(Int.toFloat(counter), 5.0) === 0.0
        let isDivisibleBy7 = Float.mod(Int.toFloat(counter), 7.0) === 0.0

        sum(
          isDivisibleBy3 || isDivisibleBy5 || isDivisibleBy7 ? answer + counter : answer,
          counter + 1,
        )
      }
    }
  }

  sum(0, 1)
}

let n1 = 7
let r1 = sumMultiples(n1)
Console.log2("r1: ", r1) // 21

let n2 = 10
let r2 = sumMultiples(n2)
Console.log2("r2: ", r2) // 40

let n3 = 9
let r3 = sumMultiples(n3)
Console.log2("r3: ", r3) // 30
