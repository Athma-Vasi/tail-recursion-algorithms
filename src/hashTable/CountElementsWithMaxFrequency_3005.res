// T(n) = O(n)
// S(n) = O(n)

let countElementsWithMaxFrequency = (nums: array<int>) => {
  let rec makeFreqTable = (freqTable: Map.t<int, int>, index: int) => {
    switch index === Array.length(nums) {
    | true => freqTable
    | false => {
        let num = switch nums->Array.at(index) {
        | None => 0
        | Some(n) => n
        }
        let existingCount = switch freqTable->Map.get(num) {
        | None => 0
        | Some(c) => c
        }
        freqTable->Map.set(num, existingCount + 1)

        makeFreqTable(freqTable, index + 1)
      }
    }
  }

  let freqTable = makeFreqTable(Map.make(), 0)

  let freqValues = freqTable->Map.values->Core__Iterator.toArray
  let maxFreq =
    freqValues->Array.reduce(Int32.min_int, (maxFreq, freq) => freq > maxFreq ? freq : maxFreq)
  let maxFreqCount =
    freqValues->Array.reduce(0, (maxFreqCount, freq) =>
      freq === maxFreq ? maxFreqCount + 1 : maxFreqCount
    )

  maxFreq * maxFreqCount
}

let n1 = [1, 2, 2, 3, 1, 4]
let r1 = countElementsWithMaxFrequency(n1)
Console.log2("r1: ", r1) // 4

let n2 = [1, 2, 3, 4, 5]
let r2 = countElementsWithMaxFrequency(n2)
Console.log2("r2: ", r2) // 5
