// T(n) = O(n)
// S(n) = O(n)

let splitStringsBySeparator = (words: array<string>, separator: string) => {
  let rec splitStrings = (result: array<string>, wordsIndex: int) => {
    switch wordsIndex === Array.length(words) {
    | true => result
    | false => {
        let word = words->Array.at(wordsIndex)->Option.mapOr(String.make(), w => w)

        let rec wordLoop = (tempResult: array<string>, stack: string, charIndex: int) => {
          switch charIndex === String.length(word) {
          | true => tempResult->Array.concat([stack])
          | false => {
              let char = word->String.charAt(charIndex)

              switch char === separator {
              | true => wordLoop(tempResult->Array.concat([stack]), String.make(), charIndex + 1)
              | false => wordLoop(tempResult, stack->String.concat(char), charIndex + 1)
              }
            }
          }
        }

        let tempResult = wordLoop([], String.make(), 0)
        splitStrings(result->Array.concat(tempResult), wordsIndex + 1)
      }
    }
  }

  splitStrings([], 0)->Array.filter(w => String.length(w) !== 0)
}

let w1 = ["one.two.three", "four.five", "six"]
let s1 = "."
let r1 = splitStringsBySeparator(w1, s1)
Console.log2("r1: ", r1) // ["one", "two", "three", "four", "five", "six"]

let w2 = ["$easy$", "$problem$"]
let s2 = "$"
let r2 = splitStringsBySeparator(w2, s2)
Console.log2("r2: ", r2) // ["easy", "problem"]

let w3 = ["|||"]
let s3 = "|"
let r3 = splitStringsBySeparator(w3, s3)
Console.log2("r3: ", r3) // []
