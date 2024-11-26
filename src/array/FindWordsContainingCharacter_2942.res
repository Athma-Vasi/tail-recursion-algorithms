// T(n) = O(n * m)
// S(n) = O(n)

let findWordsContainingCharacter = (words: array<string>, x: string) => {
  let rec findIndices = (indices: array<int>, wordsIndex: int) => {
    switch wordsIndex === Array.length(words) {
    | true => indices
    | false => {
        let word = switch words->Array.at(wordsIndex) {
        | None => String.make()
        | Some(w) => w
        }

        let rec wordLoop = (index: int, charIndex: int) => {
          switch charIndex === String.length(word) {
          | true => index
          | false =>
            wordLoop(word->String.charAt(charIndex) === x ? wordsIndex : index, charIndex + 1)
          }
        }

        let index = wordLoop(-1, 0)
        index < 0
          ? findIndices(indices, wordsIndex + 1)
          : findIndices(indices->Array.concat([index]), wordsIndex + 1)
      }
    }
  }

  findIndices([], 0)
}

let w1 = ["leet", "code"]
let x1 = "e"
let r1 = findWordsContainingCharacter(w1, x1)
Console.log2("r1: ", r1) // [0, 1]

let w2 = ["abc", "bcd", "aaaa", "cbc"]
let x2 = "a"
let r2 = findWordsContainingCharacter(w2, x2)
Console.log2("r2: ", r2) // [0, 2]

let w3 = ["abc", "bcd", "aaaa", "cbc"]
let x3 = "z"
let r3 = findWordsContainingCharacter(w3, x3)
Console.log2("r3: ", r3) // []
