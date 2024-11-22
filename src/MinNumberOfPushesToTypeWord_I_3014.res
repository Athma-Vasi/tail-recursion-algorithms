// T(n) = O(n)
// S(n) = O(n)
// INCORRECT

let minNumberOfPushesToTypeWord_I = (word: string) => {
  let rec findFrequency = (freqTable: Map.t<string, int>, index: int) => {
    switch index === String.length(word) {
    | true => freqTable
    | false => {
        let char = word->String.charAt(index)
        let existingCount = switch freqTable->Map.get(char) {
        | None => 0
        | Some(c) => c
        }
        freqTable->Map.set(char, existingCount + 1)

        findFrequency(freqTable, index + 1)
      }
    }
  }

  let freqTable = findFrequency(Map.make(), 0)

  let sorted =
    freqTable
    ->Map.entries
    ->Core__Iterator.toArray
    ->Array.toSorted(((_a1, b1), (_a2, b2)) => {Int.compare(b2, b1)})

  let rec minimumPushes = (keyPress: int, sorted: array<(string, int)>, index: int) => {
    switch index === Array.length(sorted) {
    | true => keyPress
    | false => {
        let (_char, freq) = switch sorted->Array.at(index) {
        | None => (String.make(), 0)
        | Some(t) => t
        }

        minimumPushes(freq * (index / 8 + 1), sorted, index + 1)
      }
    }
  }

  minimumPushes(0, sorted, 0)
}

let w1 = "abcde"
let r1 = minNumberOfPushesToTypeWord_I(w1)
Console.log2("r1: ", r1)

let w2 = "xycdefghij"
let r2 = minNumberOfPushesToTypeWord_I(w2)
Console.log2("r2: ", r2)
