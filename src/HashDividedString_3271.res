// T(n) = O(n)
// S(n) = O(1)

let hashDividedString = (str: string, k: int) => {
  let rec outerLoop = (hashedResult: string, rest: string, outerIndex: int) => {
    switch outerIndex === k || String.length(rest) === 0 {
    | true => hashedResult
    | false => {
        let rec innerLoop = (sum: float, subString: string, innerIndex: int) => {
          switch innerIndex === String.length(subString) {
          | true => sum
          | false => {
              let charACode = "a"->String.charCodeAt(0)
              let firstChar = subString->String.charAt(innerIndex)
              let firstCharPosition = firstChar->String.charCodeAt(0) -. charACode

              innerLoop(sum +. firstCharPosition, subString, innerIndex + 1)
            }
          }
        }

        let subString = rest->String.slice(~start=0, ~end=k)
        let remaining = rest->String.slice(~start=k, ~end=String.length(rest))
        let sum = innerLoop(0.0, subString, 0)
        let hashedCharPosition = Float.mod(sum, 26.0)
        let hashedChar = String.fromCharCode(Float.toInt(hashedCharPosition +. 97.0))

        outerLoop(hashedResult->String.concat(hashedChar), remaining, outerIndex + 1)
      }
    }
  }

  outerLoop("", str, 0)
}

let s1 = "abcd"
let k1 = 2
let r1 = hashDividedString(s1, k1)
Console.log2("r1: ", r1) // "bf"

let s2 = "mxz"
let k2 = 3
let r2 = hashDividedString(s2, k2)
Console.log2("r2: ", r2) // "i"
