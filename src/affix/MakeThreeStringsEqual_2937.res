// T(n) = O(n)
// S(n) = O(1)

let makeThreeStringsEqual = (s1: string, s2: string, s3: string) => {
  let rec checkPrefixes = (isEqual: bool, index: int) => {
    switch !isEqual {
    | true => index
    | false => {
        let char1 = s1->String.charAt(index)
        let char2 = s2->String.charAt(index)
        let char3 = s3->String.charAt(index)

        switch char1 === char2 && char2 === char3 {
        | true => checkPrefixes(true, index + 1)
        | false => checkPrefixes(false, index)
        }
      }
    }
  }

  let index = checkPrefixes(true, 0)

  let s1Length = String.length(s1)
  let s2Length = String.length(s2)
  let s3Length = String.length(s3)

  let opsS1 = index > 0 && index <= s1Length ? s1Length - index : 0
  let opsS2 = index > 0 && index <= s2Length ? s2Length - index : 0
  let opsS3 = index > 0 && index <= s3Length ? s3Length - index : 0
  let ops = opsS1 + opsS2 + opsS3

  ops === 0 ? -1 : ops
}

let s1 = "abc"
let s11 = "abb"
let s111 = "ab"
let r1 = makeThreeStringsEqual(s1, s11, s111)
Console.log2("r1: ", r1) // 2

let s2 = "dac"
let s22 = "bac"
let s222 = "cac"
let r2 = makeThreeStringsEqual(s2, s22, s222)
Console.log2("r2: ", r2) // -1
