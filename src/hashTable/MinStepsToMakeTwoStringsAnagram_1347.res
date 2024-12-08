// T(n) = O(n)
// S(n) = O(n)

let minStepsToMakeTwoStringsAnagram = (s: string, t: string) => {
  let rec makeFreqTable = (freqTable: Map.t<string, int>, str: string, index: int) => {
    switch index === String.length(str) {
    | true => freqTable
    | false => {
        let char = str->String.charAt(index)
        let freq = switch freqTable->Map.get(char) {
        | None => 1
        | Some(f) => f + 1
        }
        freqTable->Map.set(char, freq)

        makeFreqTable(freqTable, str, index + 1)
      }
    }
  }

  let freqTable = makeFreqTable(Map.make(), s, 0)

  let rec findMinSteps = (minSteps: int, str: string, index: int) => {
    switch index === String.length(str) {
    | true => minSteps
    | false => {
        let char = str->String.charAt(index)

        switch freqTable->Map.has(char) {
        | true => {
            let freq = switch freqTable->Map.get(char) {
            | None => 0
            | Some(f) => f - 1
            }
            freqTable->Map.set(char, freq)

            findMinSteps(freq < 0 ? minSteps + 1 : minSteps, str, index + 1)
          }
        | false => findMinSteps(minSteps + 1, str, index + 1)
        }
      }
    }
  }

  findMinSteps(0, t, 0)
}

let s1 = "bab"
let t1 = "aba"
let r1 = minStepsToMakeTwoStringsAnagram(s1, t1)
Console.log2("r1: ", r1) // 1

let s2 = "leetcode"
let t2 = "practice"
let r2 = minStepsToMakeTwoStringsAnagram(s2, t2)
Console.log2("r2: ", r2) // 5

let s3 = "anagram"
let t3 = "mangaar"
let r3 = minStepsToMakeTwoStringsAnagram(s3, t3)
Console.log2("r3: ", r3) // 0
