// T(n) = O(n)
// S(n) = O(n)

let firstOccurrenceIndex = (haystack: string, needle: string) => {
  let haystackLength = String.length(haystack)
  let needleLength = String.length(needle)

  let needleCountTable = String.split(needle, "")->Array.reduce(Map.make(), (acc, char) => {
    let existingCount = switch acc->Map.get(char) {
    | None => 0
    | Some(char) => char
    }
    acc->Map.set(char, existingCount + 1)
    acc
  })

  let rec expandWindow = (needleCountTable: Map.t<string, int>, index: int) => {
    switch index === needleLength {
    | true => needleCountTable
    | false => {
        let hayChar = switch haystack->String.get(index) {
        | None => ""
        | Some(char) => char
        }

        switch needleCountTable->Map.has(hayChar) {
        | true => {
            let existingCount = switch needleCountTable->Map.get(hayChar) {
            | None => 0
            | Some(count) => count
            }
            needleCountTable->Map.set(hayChar, existingCount - 1)
          }
        | false => ()
        }

        expandWindow(needleCountTable, index + 1)
      }
    }
  }

  let rec loop = (
    occurrences: array<int>,
    needleCountTable: Map.t<string, int>,
    leftIndex: int,
    rightIndex: int,
  ) => {
    let needleCounts =
      needleCountTable
      ->Map.entries
      ->Array.fromIterator
      ->Array.reduce(0, (acc, (_char, count)) => acc + count)

    let updatedOccurrences = switch needleCounts === 0 {
    | true => occurrences->Array.concat([leftIndex - 1])
    | false => occurrences
    }

    switch rightIndex === haystackLength {
    | true =>
      switch updatedOccurrences->Array.get(0) {
      | None => -1
      | Some(num) => num
      }

    | false => {
        let leftExcludedChar = switch haystack->String.get(leftIndex - 1) {
        | None => ""
        | Some(char) => char
        }
        switch needleCountTable->Map.has(leftExcludedChar) {
        | true => {
            let existingCount = switch needleCountTable->Map.get(leftExcludedChar) {
            | None => 0
            | Some(count) => count
            }
            needleCountTable->Map.set(leftExcludedChar, existingCount + 1)
          }
        | false => ()
        }

        let rightIncludedChar = switch haystack->String.get(rightIndex) {
        | None => ""
        | Some(char) => char
        }
        switch needleCountTable->Map.has(rightIncludedChar) {
        | true => {
            let existingCount = switch needleCountTable->Map.get(rightIncludedChar) {
            | None => 0
            | Some(count) => count
            }
            needleCountTable->Map.set(rightIncludedChar, existingCount - 1)
          }
        | false => ()
        }

        // keep window size fixed to needle length and slide
        loop(updatedOccurrences, needleCountTable, leftIndex + 1, rightIndex + 1)
      }
    }
  }

  loop([], expandWindow(needleCountTable, 0), 1, needleLength)
}

let h1 = "sadbutsad"
let n1 = "sad"
let r1 = firstOccurrenceIndex(h1, n1)
Console.log2("r1: ", r1)

let h2 = "leetcode"
let n2 = "leeta"
let r2 = firstOccurrenceIndex(h2, n2)
Console.log2("r2: ", r2)
