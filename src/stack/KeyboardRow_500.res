// T(n) = O(n)
// S(n) = O(1)

let keyboardRow = (words: array<string>) => {
  let firstRowSet = String.split("qwertyuiop", "")->Array.reduce(Set.make(), (setAcc, char) => {
    setAcc->Set.add(char)
    setAcc
  })
  let secondRowSet = String.split("asdfghjkl", "")->Array.reduce(Set.make(), (setAcc, char) => {
    setAcc->Set.add(char)
    setAcc
  })
  let thirdRowSet = String.split("zxcvbnm", "")->Array.reduce(Set.make(), (setAcc, char) => {
    setAcc->Set.add(char)
    setAcc
  })

  let rec wordsLoop = (result: array<string>, wordsIndex: int) => {
    let word = switch words->Array.at(wordsIndex) {
    | None => ""
    | Some(w) => w
    }
    let wordLength = String.length(word)

    let rec wordLoop = (
      ~firstRowStack: string,
      ~secondRowStack: string,
      ~thirdRowStack: string,
      wordIndex: int,
    ) => {
      let char = word->String.charAt(wordIndex)->String.toLowerCase

      switch wordIndex === wordLength {
      | true => (firstRowStack, secondRowStack, thirdRowStack)
      | false =>
        wordLoop(
          ~firstRowStack=firstRowSet->Set.has(char)
            ? firstRowStack->String.concat(char)
            : firstRowStack,
          ~secondRowStack=secondRowSet->Set.has(char)
            ? secondRowStack->String.concat(char)
            : secondRowStack,
          ~thirdRowStack=thirdRowSet->Set.has(char)
            ? thirdRowStack->String.concat(char)
            : thirdRowStack,
          wordIndex + 1,
        )
      }
    }

    let (firstRowStack, secondRowStack, thirdRowStack) = wordLoop(
      ~firstRowStack="",
      ~secondRowStack="",
      ~thirdRowStack="",
      0,
    )

    switch wordsIndex === Array.length(words) {
    | true => result
    | false =>
      wordsLoop(
        String.length(firstRowStack) === wordLength ||
        String.length(secondRowStack) === wordLength ||
        String.length(thirdRowStack) === wordLength
          ? result->Array.concat([word])
          : result,
        wordsIndex + 1,
      )
    }
  }

  wordsLoop([], 0)
}

let w1 = ["Hello", "Alaska", "Dad", "Peace"]
let r1 = keyboardRow(w1)
Console.log2("r1: ", r1) // ["Alaska", "Dad"]

let w2 = ["omk"]
let r2 = keyboardRow(w2)
Console.log2("r2: ", r2) // []

let w3 = ["adsdf", "sfd"]
let r3 = keyboardRow(w3)
Console.log2("r3: ", r3) // ["adsdf", "sfd"]
