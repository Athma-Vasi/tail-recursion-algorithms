// INCORRECT

let findAndReplacePattern = (words: array<string>, pattern: string) => {
  let rec wordsLoop = (result: array<string>, wordsIndex: int) => {
    switch wordsIndex === Array.length(words) {
    | true => result
    | false => {
        let word = words->Array.at(wordsIndex)->Option.map(w => w)->Option.getOr(String.make())

        let rec wordLoop = (wordToPatternMap: Map.t<string, string>, charIndex: int) => {
          switch charIndex === String.length(word) {
          | true =>
            Map.size(wordToPatternMap) === Map.values(wordToPatternMap)->Set.fromIterator->Set.size
          | false => {
              let wordChar = word->String.charAt(charIndex)
              let patternChar = pattern->String.charAt(charIndex)
              let mappedChar =
                wordToPatternMap->Map.get(wordChar)->Option.map(c => c)->Option.getOr(String.make())

              switch mappedChar === patternChar {
              | true => {
                  wordToPatternMap->Map.set(wordChar, patternChar)
                  wordLoop(wordToPatternMap, charIndex + 1)
                }
              | false => wordLoop(wordToPatternMap, charIndex + 1)
              }
            }
          }
        }

        let isPattern = wordLoop(Map.make(), 0)
        wordsLoop(isPattern ? result->Array.concat([word]) : result, wordsIndex + 1)
      }
    }
  }

  wordsLoop([], 0)
}

let w1 = ["abc", "deq", "mee", "aqq", "dkd", "ccc"]
let p1 = "abb"
let r1 = findAndReplacePattern(w1, p1)
Console.log2("r1: ", r1) // ["mee","aqq"]

let w2 = ["a", "b", "c"]
let p2 = "a"
let r2 = findAndReplacePattern(w2, p2)
Console.log2("r2: ", r2) // ["a","b","c"]
