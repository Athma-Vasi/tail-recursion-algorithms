// T(n) = O(n)
// S(n) = O(n)

let circularSentence = (sentence: string) => {
  let rec loop = (prevLast: string, words: array<string>) => {
    switch Array.length(words) === 0 {
    | true => true
    | false => {
        let word = switch words->Array.at(0) {
        | None => ""
        | Some(c) => c
        }

        let first = word->String.charAt(0)
        let currLast = word->String.charAt(String.length(word) - 1)

        prevLast === first && loop(currLast, words->Array.slice(~start=1, ~end=Array.length(words)))
      }
    }
  }

  let firstChar = sentence->String.charAt(0)
  let lastChar = sentence->String.charAt(String.length(sentence) - 1)
  let words = sentence->String.split(" ")
  let firstWord = switch words->Array.at(0) {
  | None => ""
  | Some(c) => c
  }
  let firstWordLastChar = firstWord->String.charAt(String.length(firstWord) - 1)

  firstChar === lastChar &&
    loop(firstWordLastChar, words->Array.slice(~start=1, ~end=Array.length(words)))
}

let s1 = "leetcode exercises sound delightful"
let r1 = circularSentence(s1)
Console.log2("r1: ", r1) // true

let s2 = "sound delightful leetcode exercises"
let r2 = circularSentence(s2)
Console.log2("r2: ", r2) // true

let s3 = "Leetcode is cool"
let r3 = circularSentence(s3)
Console.log2("r3: ", r3) // false
