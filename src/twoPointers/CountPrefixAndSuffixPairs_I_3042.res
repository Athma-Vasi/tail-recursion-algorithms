let countPrefixAndSuffixPairs_I = (words: array<string>) => {
  let length = Array.length(words)

  let rec countEqualAffixes = (count: int, leftIndex: int, rightIndex: int) => {
    switch leftIndex > rightIndex || rightIndex === length {
    | true => count
    | false =>
      switch leftIndex === rightIndex {
      | true => countEqualAffixes(count, leftIndex + 1, length - 1)
      | false => {
          let leftWord = switch words->Array.at(leftIndex) {
          | None => String.make()
          | Some(w) => w
          }
          let rightWord = switch words->Array.at(rightIndex) {
          | None => String.make()
          | Some(w) => w
          }

          let checkAffixesPairs = (leftWord: string, rightWord: string) => {
            let rec checkPrefixes = (
              arePrefixesEqual: Set.t<bool>,
              leftWord: string,
              rightWord: string,
              left: int,
            ) => {
              switch left === String.length(leftWord) {
              | true => !Set.has(arePrefixesEqual, false)
              | false => {
                  let leftChar = leftWord->String.charAt(left)
                  let rightChar = rightWord->String.charAt(left)
                  arePrefixesEqual->Set.add(leftChar === rightChar)

                  checkPrefixes(arePrefixesEqual, leftWord, rightWord, left + 1)
                }
              }
            }

            let rec checkSuffixes = (
              areSuffixesEqual: Set.t<bool>,
              leftWord: string,
              rightWord: string,
              right: int,
            ) => {
              switch right < 0 {
              | true => !Set.has(areSuffixesEqual, false)
              | false => {
                  let leftChar = leftWord->String.charAt(right)
                  let rightChar = rightWord->String.charAt(right)
                  areSuffixesEqual->Set.add(leftChar === rightChar)

                  checkSuffixes(areSuffixesEqual, leftWord, rightWord, right - 1)
                }
              }
            }

            let arePrefixesEqual = checkPrefixes(Set.make(), leftWord, rightWord, 0)
            let areSuffixesEqual = checkSuffixes(
              Set.make(),
              leftWord,
              rightWord,
              String.length(leftWord) - 1,
            )

            arePrefixesEqual && areSuffixesEqual
          }

          let areAffixesEqual = checkAffixesPairs(leftWord, rightWord)
          countEqualAffixes(areAffixesEqual ? count + 1 : count, leftIndex, rightIndex - 1)
        }
      }
    }
  }

  countEqualAffixes(0, 0, length - 1)
}

let w1 = ["a", "aba", "ababa", "aa"]
let r1 = countPrefixAndSuffixPairs_I(w1)
Console.log2("r1: ", r1) // 4

let w2 = ["pa", "papa", "ma", "mama"]
let r2 = countPrefixAndSuffixPairs_I(w2)
Console.log2("r2: ", r2) // 2

let w3 = ["abab", "ab"]
let r3 = countPrefixAndSuffixPairs_I(w3)
Console.log2("r3: ", r3) // 0
