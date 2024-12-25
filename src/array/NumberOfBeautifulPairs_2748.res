// T(n) = O(n^2)
// S(n) = O(1)

type position = First | Last

let returnDigit = (num: int, position: position) => {
  let numStr = Int.toString(num)
  let length = String.length(numStr)
  switch (position, length > 2) {
  | (First, true) => numStr->String.slice(~start=0, ~end=2)->Int.fromString
  | (Last, true) => numStr->String.slice(~start=length - 2, ~end=length)->Int.fromString
  | _ => Some(num)
  }->Option.mapOr(0, n => n)
}

let greatestCommonDivisor = (a: int, b: int) => {
  let rec gcd = (answer: int, remainder: int) => {
    switch remainder === 0 {
    | true => answer
    | false => gcd(remainder, Float.mod(Int.toFloat(answer), Int.toFloat(remainder))->Int.fromFloat)
    }
  }

  gcd(a, b)
}

let numberOfBeautifulPairs = (nums: array<int>) => {
  let length = Array.length(nums)

  let rec outerLoop = (outerCount: int, outerIndex: int) => {
    switch outerIndex === length {
    | true => outerCount
    | false => {
        let outerNum = nums->Array.at(outerIndex)->Option.mapOr(0, n => n)
        let first = returnDigit(outerNum, First)

        let rec innerLoop = (innerCount: int, innerIndex: int) => {
          switch innerIndex === length {
          | true => innerCount
          | false => {
              let innerNum = nums->Array.at(innerIndex)->Option.mapOr(0, n => n)
              let last = returnDigit(innerNum, Last)
              let gcd = greatestCommonDivisor(first, last)

              innerLoop(gcd === 1 ? innerCount + 1 : innerCount, innerIndex + 1)
            }
          }
        }

        let innerCount = innerLoop(outerCount, outerIndex + 1)
        outerLoop(innerCount, outerIndex + 1)
      }
    }
  }

  outerLoop(0, 0)
}

let n1 = [2, 5, 1, 4]
let r1 = numberOfBeautifulPairs(n1)
Console.log2("r1: ", r1) // 5

let n2 = [11, 21, 12]
let r2 = numberOfBeautifulPairs(n2)
Console.log2("r2: ", r2) // 2
