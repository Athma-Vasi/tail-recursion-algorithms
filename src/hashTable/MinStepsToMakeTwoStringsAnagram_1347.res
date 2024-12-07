// INCOMPLETE

let minStepsToMakeTwoStringsAnagram = (s: string, t: string) => {
  let rec makeFreqTable = (
    freqTable: Map.t<string, int>,
    str1: string,
    str2: string,
    index: int,
  ) => {
    switch index === String.length(str1) {
    | true => freqTable
    | false => {
        let char1 = str1->String.charAt(index)
        let char2 = str2->String.charAt(index)
        let freq1 = switch freqTable->Map.get(char1) {
        | None => 1
        | Some(f) => f + 1
        }
        let freq2 = switch freqTable->Map.get(char2) {
        | None => 0
        | Some(f) => f
        }
        freqTable->Map.set(char1, freq1 - freq2)

        makeFreqTable(freqTable, str1, str2, index + 1)
      }
    }
  }

  let freqTable = makeFreqTable(Map.make(), s, t, 0)

  Console.log("\n")
  Console.log2("freqTable: ", freqTable)

  let rec findMinSteps = (minSteps: int, tuples: array<(string, int)>, index: int) => {
    switch index === Array.length(tuples) {
    | true => minSteps
    | false => {
        let (char, freq) = switch tuples->Array.at(index) {
        | None => (String.make(), 0)
        | Some(t) => t
        }

        Console.log("\n")
        Console.log("--findMinSteps--")
        Console.log2("minSteps: ", minSteps)
        Console.log2("tuples: ", tuples)
        Console.log2("index: ", index)
        Console.log2("char: ", char)
        Console.log2("freq: ", freq)

        findMinSteps(minSteps + freq, tuples, index + 1)
      }
    }
  }

  findMinSteps(0, freqTable->Map.entries->Array.fromIterator, 0)
}

let s1 = "bab"
let t1 = "aba"
let r1 = minStepsToMakeTwoStringsAnagram(s1, t1)
Console.log2("r1: ", r1) // 1

// let s2 = "leetcode"
// let t2 = "practice"
// let r2 = minStepsToMakeTwoStringsAnagram(s2, t2)
// Console.log2("r2: ", r2) // 5

// let s3 = "anagram"
// let t3 = "mangaar"
// let r3 = minStepsToMakeTwoStringsAnagram(s3, t3)
// Console.log2("r3: ", r3) // 0
