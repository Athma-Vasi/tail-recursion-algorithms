// T(n) = O(n)
// S(n) = O(n)

let findCommonElementsBetweenTwoArrays = (nums1: array<int>, nums2: array<int>) => {
  let rec makeFreqTable = (freqTable: Map.t<int, int>, nums: array<int>, index: int) => {
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

        makeFreqTable(freqTable, nums, index + 1)
      }
    }
  }

  let freqTable1 = makeFreqTable(Map.make(), nums1, 0)
  let freqTable2 = makeFreqTable(Map.make(), nums2, 0)

  let nums1Set = Set.fromArray(nums1)
  let nums2Set = Set.fromArray(nums2)

  let count1 =
    freqTable1
    ->Map.entries
    ->Array.fromIterator
    ->Array.reduce(0, (acc, tuple) => {
      let (num, freq) = tuple
      nums2Set->Set.has(num) ? acc + freq : acc
    })

  let count2 =
    freqTable2
    ->Map.entries
    ->Array.fromIterator
    ->Array.reduce(0, (acc, tuple) => {
      let (num, freq) = tuple
      nums1Set->Set.has(num) ? acc + freq : acc
    })

  [count1, count2]
}

let n1 = [2, 3, 2]
let n11 = [1, 2]
let r1 = findCommonElementsBetweenTwoArrays(n1, n11)
Console.log2("r1: ", r1) // [2, 1]

let n2 = [4, 3, 2, 3, 1]
let n22 = [2, 2, 5, 2, 3, 6]
let r2 = findCommonElementsBetweenTwoArrays(n2, n22)
Console.log2("r2: ", r2) // [3, 4]

let n3 = [3, 4, 2, 3]
let n33 = [1, 5]
let r3 = findCommonElementsBetweenTwoArrays(n3, n33)
Console.log2("r3: ", r3) // [0, 0]
