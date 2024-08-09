// T(n) = O(n)
// S(n) = O(n)

let firstUniqueCharInString = (str: string) => {
  let strLength = String.length(str)

  let rec makeTableAndQueue = (
    charQueue: array<string>,
    freqTable: Map.t<string, int>,
    index: int,
  ) => {
    switch index === strLength {
    | true => (charQueue, freqTable)
    | false => {
        let char = str->String.charAt(index)
        let existingFreq = switch freqTable->Map.get(char) {
        | None => 0
        | Some(count) => count
        }
        freqTable->Map.set(char, existingFreq + 1)

        makeTableAndQueue(charQueue->Array.concat([char]), freqTable, index + 1)
      }
    }
  }

  let (charQueue, freqTable) = makeTableAndQueue([], Map.make(), 0)

  let rec queueLoop = (charQueue: array<string>, index: int) => {
    let queueLength = Array.length(charQueue)

    switch queueLength === 0 {
    | true => -1
    | false => {
        let firstChar = switch charQueue->Array.at(0) {
        | None => ""
        | Some(char) => char
        }

        let charFreq = switch freqTable->Map.get(firstChar) {
        | None => -1
        | Some(count) => count
        }

        charFreq === 1
          ? index
          : queueLoop(charQueue->Array.slice(~start=1, ~end=queueLength), index + 1)
      }
    }
  }

  queueLoop(charQueue, 0)
}

let s1 = "leetcode"
let r1 = firstUniqueCharInString(s1)
Console.log2("r1: ", r1) // 0

let s2 = "loveleetcode"
let r2 = firstUniqueCharInString(s2)
Console.log2("r2: ", r2) // 2

let s3 = "aabb"
let r3 = firstUniqueCharInString(s3)
Console.log2("r3: ", r3) // -1
