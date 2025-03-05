// T(a,b) = O(min(a,b))
// S(a,b) = O(min(a,b))

let numberOfCommonFactors = (a: int, b: int) => {
  let smaller = a < b ? a : b

  let rec find = (factors: Set.t<int>, n: int) => {
    switch n > smaller {
    | true => Set.size(factors)
    | false => {
        let nFloat = Int.toFloat(n)
        let isFactor =
          Float.mod(Int.toFloat(a), nFloat) === 0.0 && Float.mod(Int.toFloat(b), nFloat) === 0.0
        isFactor ? factors->Set.add(n) : ()

        find(factors, n + 1)
      }
    }
  }

  find(Set.make(), 1)
}

let a1 = 12
let b1 = 6
let r1 = numberOfCommonFactors(a1, b1)
Console.log2("r1: ", r1) // 4

let a2 = 25
let b2 = 30
let r2 = numberOfCommonFactors(a2, b2)
Console.log2("r2: ", r2) // 2
