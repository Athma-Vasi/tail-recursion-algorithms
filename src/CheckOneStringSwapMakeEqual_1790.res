// T(n) = O(n)
// S(n) = O(1)

let checkOneStringSwapMakeEqual = (str1: string, str2: string) => {
  let rec loop = (amountOfDiffs: int, index: int) => {
    str1 === str2 ||
      switch index === String.length(str1) {
      | true => amountOfDiffs === 2
      | false =>
        str1->String.charAt(index) === str2->String.charAt(index)
          ? loop(amountOfDiffs, index + 1)
          : loop(amountOfDiffs + 1, index + 1)
      }
  }

  loop(0, 0)
}

let s1 = "bank"
let s11 = "kanb"
let r1 = checkOneStringSwapMakeEqual(s1, s11) // true
Console.log2("r1: ", r1)

let s2 = "attack"
let s22 = "defend"
let r2 = checkOneStringSwapMakeEqual(s2, s22) // false
Console.log2("r2: ", r2)

let s3 = "kelb"
let s33 = "kelb"
let r3 = checkOneStringSwapMakeEqual(s3, s33) // true
Console.log2("r3: ", r3)
