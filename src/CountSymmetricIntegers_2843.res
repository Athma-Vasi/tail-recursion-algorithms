// T(n) = O(n)
// S(n) = O(1)

let countSymmetricIntegers = (low: int, high: int) => {
  let splitInteger = (num: int) => {
    let numStr = Int.toString(num)
    let numLength = String.length(numStr)
    let n = numLength / 2 - 1

    let rec loop = (left: string, right: string, index: int) => {
      switch index === numLength {
      | true => (left, right)
      | false => {
          let numChar = numStr->String.charAt(index)

          switch index <= n {
          | true => loop(left->String.concat(numChar), right, index + 1)
          | false => loop(left, right->String.concat(numChar), index + 1)
          }
        }
      }
    }

    loop(String.make(), String.make(), 0)
  }

  let findSumOfInteger = (num: int) =>
    Int.toString(num)
    ->String.split("")
    ->Array.reduce(0, (acc, nChar) => {
      let num = switch Int.fromString(nChar) {
      | None => 0
      | Some(n) => n
      }
      acc + num
    })

  let rec count = (amount: int, num: int) => {
    switch num === high + 1 {
    | true => amount
    | false => {
        let isNumDigitsAmountEven =
          Float.mod(Int.toString(num)->String.length->Int.toFloat, 2.0) === 0.0

        switch isNumDigitsAmountEven {
        | true => {
            let (left, right) = splitInteger(num)
            let leftNum = switch Int.fromString(left) {
            | None => 0
            | Some(n) => n
            }
            let rightNum = switch Int.fromString(right) {
            | None => 0
            | Some(n) => n
            }
            let leftSum = findSumOfInteger(leftNum)
            let rightSum = findSumOfInteger(rightNum)

            count(leftSum === rightSum ? amount + 1 : amount, num + 1)
          }
        | false => count(amount, num + 1)
        }
      }
    }
  }

  count(0, low)
}

let l1 = 1
let h1 = 100
let r1 = countSymmetricIntegers(l1, h1)
Console.log2("r1: ", r1) // 9

let l2 = 1200
let h2 = 1230
let r2 = countSymmetricIntegers(l2, h2)
Console.log2("r2: ", r2) // 4
