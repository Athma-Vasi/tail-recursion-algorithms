// T(n) = O(n^2)
// S(n) = O(n)

let removeAllOccurrencesOfASubstring = (str: string, substr: string) => {
  let makeSubstrFreqTable = substr =>
    substr
    ->String.split("")
    ->Array.reduce(Map.make(), (map, char) => {
      map->Map.set(char, 0)
      map
    })

  let rec expandWindow = (freqTable: Map.t<string, int>, sliced: string, index: int) => {
    switch index === String.length(substr) {
    | true => freqTable
    | false => {
        let char = sliced->String.charAt(index)
        switch freqTable->Map.has(char) {
        | true => {
            let freq = freqTable->Map.get(char)->Option.map(f => f + 1)->Option.getOr(1)
            freqTable->Map.set(char, freq)
          }
        | false => ()
        }

        expandWindow(freqTable, sliced, index + 1)
      }
    }
  }

  let substrLength = String.length(substr)

  let checkIfSubstrPresent = freqTable => {
    let (chars, charsCount) =
      freqTable
      ->Map.entries
      ->Array.fromIterator
      ->Array.reduce((0, 0), (acc, entry) => {
        let (chars, charsCount) = acc
        let (_char, freq) = entry
        freq === 1 ? (chars + 1, charsCount + 1) : (chars + 1, charsCount)
      })

    chars === substrLength && charsCount === substrLength
  }

  let rec slideWindow = (result: string, freqTable, leftIndex: int, rightIndex: int) => {
    switch rightIndex === String.length(result) {
    | true => result
    | false => {
        let leftExcludedChar = result->String.charAt(leftIndex)
        let rightIncludedChar = result->String.charAt(rightIndex + 1)
        let leftFreq = switch freqTable->Map.get(leftExcludedChar) {
        | None => -1
        | Some(f) => f - 1
        }
        leftFreq >= 0 ? freqTable->Map.set(leftExcludedChar, leftFreq) : ()
        let rightFreq = switch freqTable->Map.get(rightIncludedChar) {
        | None => -1
        | Some(f) => f + 1
        }
        rightFreq >= 0 ? freqTable->Map.set(rightIncludedChar, rightFreq) : ()
        let isSubstrPresent = checkIfSubstrPresent(freqTable)

        switch isSubstrPresent {
        | true => {
            let sliced =
              result
              ->String.slice(~start=0, ~end=leftIndex + 1)
              ->String.concat(result->String.sliceToEnd(~start=rightIndex + 2))
            let newFreqTable = expandWindow(makeSubstrFreqTable(substr), sliced, 0)

            slideWindow(sliced, newFreqTable, 0, substrLength - 1)
          }
        | false => slideWindow(result, freqTable, leftIndex + 1, rightIndex + 1)
        }
      }
    }
  }

  let freqTable = expandWindow(makeSubstrFreqTable(substr), str, 0)
  slideWindow(str, freqTable, 0, substrLength - 1)
}

let s1 = "daabcbaabcbc"
let s11 = "abc"
let r1 = removeAllOccurrencesOfASubstring(s1, s11)
Console.log2("r1: ", r1) // "dab"

let s2 = "axxxxyyyyb"
let s22 = "xy"
let r2 = removeAllOccurrencesOfASubstring(s2, s22)
Console.log2("r2: ", r2) // "ab"
