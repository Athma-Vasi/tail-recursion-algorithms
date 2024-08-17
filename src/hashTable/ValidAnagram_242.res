// T(n) = O(n)
// S(n) = O(n)

let validAnagram = (s: string, t: string) => {
  let rec modifyFreqTable = (freqTable: Map.t<string, int>, index: int) => {
    switch index === String.length(t) {
    | true => freqTable
    | false => {
        let tChar = t->String.charAt(index)
        let existingCount = switch freqTable->Map.get(tChar) {
        | None => 0
        | Some(c) => c
        }

        switch existingCount === 0 {
        | true => Map.make()
        | false => {
            freqTable->Map.set(tChar, existingCount - 1)
            modifyFreqTable(freqTable, index + 1)
          }
        }
      }
    }
  }

  let freqTable = String.split(s, "")->Array.reduce(Map.make(), (acc, char) => {
    let existingCount = switch acc->Map.get(char) {
    | None => 0
    | Some(c) => c
    }
    acc->Map.set(char, existingCount + 1)
    acc
  })

  let modifiedFreqTable = modifyFreqTable(freqTable, 0)
  let sum =
    Map.size(modifiedFreqTable) === 0
      ? -1
      : modifiedFreqTable
        ->Map.values
        ->Array.fromIterator
        ->Array.reduce(0, (acc, count) => acc + count)

  sum === 0
}

let s1 = "anagram"
let s11 = "nagaram"
let r1 = validAnagram(s1, s11)
Console.log2("r1: ", r1) // true

let s2 = "rat"
let s22 = "car"
let r2 = validAnagram(s2, s22)
Console.log2("r2: ", r2) // false

let s3 = "a"
let s33 = "ab"
let r3 = validAnagram(s3, s33)
Console.log2("r3: ", r3) // false

let s4 = "a"
let s44 = "a"
let r4 = validAnagram(s4, s44)
Console.log2("r4: ", r4) // true
