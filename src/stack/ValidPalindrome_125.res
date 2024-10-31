// T(n) = O(n)
// S(n) = O(n)

let validPalindrome = (str: string) => {
  let alphaNumericSet = String.split(
    "abcdefghijklmnopqrstuvwxyz0123456789",
    "",
  )->Array.reduce(Set.make(), (set, char) => {
    set->Set.add(char)
    set
  })

  let alphaNumericStr =
    String.toLowerCase(str)
    ->String.split("")
    ->Array.reduce("", (acc, char) => {
      let newStr = alphaNumericSet->Set.has(char) ? acc->String.concat(char) : acc
      newStr
    })

  let rec loop = (sliced: string) => {
    switch String.length(sliced) < 2 {
    | true => true
    | false => {
        let slicedLength = String.length(sliced)
        let firstChar = sliced->String.charAt(0)
        let lastChar = sliced->String.charAt(slicedLength - 1)

        // switch firstChar === lastChar {
        // | true => loop(sliced->String.slice(~start=1, ~end=slicedLength - 1))
        // | false => false
        // }

        firstChar === lastChar && loop(sliced->String.slice(~start=1, ~end=slicedLength - 1))
      }
    }
  }

  loop(alphaNumericStr)
}

let s1 = "A man, a plan, a canal: Panama"
let r1 = validPalindrome(s1)
Console.log2("r1: ", r1) // true

let s2 = "race a car"
let r2 = validPalindrome(s2)
Console.log2("r2: ", r2) // false

let s3 = ""
let r3 = validPalindrome(s3)
Console.log2("r3: ", r3) // true
