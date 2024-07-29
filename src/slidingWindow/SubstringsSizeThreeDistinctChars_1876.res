// T(n) = O(n)
// S(n) = O(1)

let substringsSizeThreeDistinctChars = (str: string) => {
  let rec expandWindow = (charStack: string, index: int) => {
    switch index === 3 {
    | true => {
        let firstChar = charStack->String.charAt(0)
        let secondChar = charStack->String.charAt(1)
        let thirdChar = charStack->String.charAt(2)

        firstChar === secondChar || secondChar === thirdChar || firstChar === thirdChar ? 0 : 1
      }
    | false => {
        let char = str->String.charAt(index)
        expandWindow(charStack->String.concat(char), index + 1)
      }
    }
  }

  let rec slideWindow = (goodSubstringsCount: int, leftIndex: int, rightIndex: int) => {
    switch rightIndex === String.length(str) {
    | true => goodSubstringsCount
    | false => {
        let windowStr =
          str
          ->String.slice(~start=leftIndex, ~end=leftIndex + 3)
          ->String.concat(str->String.charAt(rightIndex))

        let firstChar = windowStr->String.charAt(0)
        let secondChar = windowStr->String.charAt(1)
        let thirdChar = windowStr->String.charAt(2)

        slideWindow(
          firstChar === secondChar || secondChar === thirdChar || firstChar === thirdChar
            ? goodSubstringsCount
            : goodSubstringsCount + 1,
          leftIndex + 1,
          rightIndex + 1,
        )
      }
    }
  }

  slideWindow(expandWindow("", 0), 1, 3)
}

let s1 = "xyzzaz"
let r1 = substringsSizeThreeDistinctChars(s1)
Console.log2("r1: ", r1)

let s2 = "aababcabc"
let r2 = substringsSizeThreeDistinctChars(s2)
Console.log2("r2: ", r2)

let s3 = "abc"
let r3 = substringsSizeThreeDistinctChars(s3)
Console.log2("r3: ", r3)
