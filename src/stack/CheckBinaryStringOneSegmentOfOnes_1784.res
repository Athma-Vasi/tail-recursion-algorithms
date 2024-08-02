// T(n) = O(n)
// S(n) = O(1)

let checkBinaryStringOneSegmentOfOnes = (str: string) => {
  let rec loop = (amountOfOneSegments: int, onesStack: string, index: int) => {
    switch index === String.length(str) {
    | true => amountOfOneSegments === 1
    | false => {
        let currentBinary = str->String.charAt(index)

        switch currentBinary === "0" {
        | true =>
          String.length(onesStack) > 1
            ? loop(amountOfOneSegments + 1, "", index + 1)
            : loop(amountOfOneSegments, "", index + 1)
        | false => loop(amountOfOneSegments, onesStack->String.concat(currentBinary), index + 1)
        }
      }
    }
  }

  loop(0, "", 0)
}

let s1 = "1001"
let r1 = checkBinaryStringOneSegmentOfOnes(s1) // false
Console.log2("r1: ", r1)

let s2 = "110"
let r2 = checkBinaryStringOneSegmentOfOnes(s2) // true
Console.log2("r2: ", r2)
