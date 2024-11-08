// T(n) = O(n^2) // unsure as js output has nested while loops...
// S(n) = O(1)

let checkBalancedString = (numStr: string) => {
  let rec evenLoop = (evenSum: int, oddSum: int, index: int) => {
    let isIndexEven = Float.mod(Int.toFloat(index), 2.0) === 0.0

    switch index === String.length(numStr) {
    | true => (evenSum, oddSum)
    | false =>
      switch isIndexEven {
      | true => {
          let numChar = numStr->String.charAt(index)
          let num = switch Int.fromString(numChar) {
          | None => -1
          | Some(n) => n
          }

          evenLoop(evenSum + num, oddSum, index + 1)
        }
      | false => oddLoop(evenSum, oddSum, index)
      }
    }
  }

  and oddLoop = (evenSum: int, oddSum: int, index: int) => {
    let isIndexOdd = Float.mod(Int.toFloat(index), 2.0) !== 0.0

    switch index === String.length(numStr) {
    | true => (evenSum, oddSum)
    | false =>
      switch isIndexOdd {
      | true => {
          let numChar = numStr->String.charAt(index)
          let num = switch Int.fromString(numChar) {
          | None => -1
          | Some(n) => n
          }

          oddLoop(evenSum, oddSum + num, index + 1)
        }
      | false => evenLoop(evenSum, oddSum, index)
      }
    }
  }

  let (evenSum, oddSum) = evenLoop(0, 0, 0)
  evenSum === oddSum
}

let s1 = "1234"
let r1 = checkBalancedString(s1)
Console.log2("r1: ", r1) // false

let s2 = "24123"
let r2 = checkBalancedString(s2)
Console.log2("r2: ", r2) // true
