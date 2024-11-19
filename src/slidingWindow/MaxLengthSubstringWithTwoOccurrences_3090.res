// T(n) = O(n)
// S(n) = O(n)
// INCORRECT

let maxLengthSubstringWithTwoOccurrences = (str: string) => {
  let uniqueChars =
    str
    ->String.split("")
    ->Array.reduce(Set.make(), (set, char) => {
      set->Set.add(char)
      set
    })
  let uniqueCharsAmount = Set.size(uniqueChars)

  let checkOccurrenceOverTwo = (freqTable: Map.t<string, int>) => {
    freqTable
    ->Map.values
    ->Core__Iterator.toArray
    ->Array.reduce(Set.make(), (set, count) => {
      set->Set.add(count > 2)
      set
    })
    ->Set.has(true)
  }

  let checkEveryCharPresent = (freqTable: Map.t<string, int>) => {
    let (charsAmount, charsCount) =
      freqTable
      ->Map.values
      ->Core__Iterator.toArray
      ->Array.reduce((0, 0), (acc, count) => {
        let (chars, counts) = acc

        (count > 0 ? chars + 1 : chars, counts + count)
      })

    // every char present
    charsAmount === uniqueCharsAmount &&
      // min one occurrence per char
      charsCount >= uniqueCharsAmount
    // &&
    // // max two occurrences per char
    // charsCount <= uniqueCharsAmount * 2
  }

  let rec slideWindow = (
    maxLength: int,
    freqTable: Map.t<string, int>,
    leftIndex: int,
    rightIndex: int,
  ) => {
    switch rightIndex === String.length(str) {
    | true => maxLength
    | false =>
      switch leftIndex === rightIndex {
      | true => slideWindow(maxLength, freqTable, leftIndex, rightIndex + 1)
      | false => {
          let rightChar = str->String.charAt(rightIndex)
          let existingCount = switch freqTable->Map.get(rightChar) {
          | None => 0
          | Some(c) => c
          }
          freqTable->Map.set(rightChar, existingCount + 1)
          let windowSize = rightIndex - leftIndex + 1
          let newMaxLength = maxLength > windowSize ? maxLength : windowSize
          let isEveryCharCorrect = checkEveryCharPresent(freqTable)
          let isOccurrenceOverTwo = checkOccurrenceOverTwo(freqTable)

          switch isOccurrenceOverTwo {
          | true => {
              let leftChar = str->String.charAt(leftIndex)
              let existingCount = switch freqTable->Map.get(leftChar) {
              | None => 0
              | Some(c) => c
              }
              freqTable->Map.set(leftChar, existingCount - 1)

              slideWindow(maxLength, freqTable, leftIndex + 1, rightIndex)
            }
          | false =>
            switch isEveryCharCorrect {
            | true => slideWindow(newMaxLength, freqTable, leftIndex, rightIndex + 1)
            | false => slideWindow(maxLength, freqTable, leftIndex, rightIndex + 1)
            }
          }
        }
      }
    }
  }

  let firstChar = str->String.charAt(0)
  let freqTable = Map.make()
  freqTable->Map.set(firstChar, 1)
  slideWindow(Int32.min_int, freqTable, 0, 1)
}

let s1 = "bcbbbcba"
let r1 = maxLengthSubstringWithTwoOccurrences(s1)
Console.log2("r1: ", r1) // 4

let s2 = "aaaa"
let r2 = maxLengthSubstringWithTwoOccurrences(s2)
Console.log2("r2: ", r2) // 2
