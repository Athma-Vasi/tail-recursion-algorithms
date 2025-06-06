// T(n) = O(n)
// S(n) = O(n)

let lexicographicallySmallestPalindrome = (str: string) => {
  let rec operate = (palindrome: string, firstIndex: int, lastIndex: int) => {
    switch lastIndex - firstIndex < 2 {
    | true => palindrome
    | false => {
        let firstChar = palindrome->String.charAt(firstIndex)
        let lastChar = palindrome->String.charAt(lastIndex)

        switch firstChar === lastChar {
        | true => operate(palindrome, firstIndex + 1, lastIndex - 1)
        | false => {
            let firstCharCode = firstChar->String.charCodeAt(0)
            let lastCharCode = lastChar->String.charCodeAt(0)

            switch firstCharCode < lastCharCode {
            | true =>
              operate(
                palindrome
                ->String.slice(~start=0, ~end=lastIndex)
                ->String.concat(firstChar)
                ->String.concat(palindrome->String.sliceToEnd(~start=lastIndex + 1)),
                firstIndex + 1,
                lastIndex - 1,
              )
            | false =>
              operate(
                palindrome
                ->String.slice(~start=0, ~end=firstIndex)
                ->String.concat(lastChar)
                ->String.concat(palindrome->String.sliceToEnd(~start=firstIndex + 1)),
                firstIndex + 1,
                lastIndex - 1,
              )
            }
          }
        }
      }
    }
  }

  operate(str, 0, String.length(str) - 1)
}

let s1 = "egcfe"
let r1 = lexicographicallySmallestPalindrome(s1)
Console.log2("r1: ", r1) // "efcfe"

let s2 = "abcd"
let r2 = lexicographicallySmallestPalindrome(s2)
Console.log2("r2: ", r2) // "abba"

let s3 = "seven"
let r3 = lexicographicallySmallestPalindrome(s3)
Console.log2("r3: ", r3) // "neven"
