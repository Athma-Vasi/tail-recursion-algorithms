// T(n) = O(n^2)
// S(n) = O(n)

let findMissingAndRepeatedValues = (grid: array<array<int>>) => {
  let yLength = Array.length(grid)
  let row = switch grid->Array.at(0) {
  | None => []
  | Some(arr) => arr
  }
  let xLength = Array.length(row)

  let rec rowLoop = (freqTable, row, xIndex: int) => {
    switch xIndex === xLength {
    | true => freqTable
    | false => {
        let num = switch row->Array.at(xIndex) {
        | None => 0
        | Some(n) => n
        }
        let existingCount = switch freqTable->Map.get(num) {
        | None => 0
        | Some(c) => c
        }
        freqTable->Map.set(num, existingCount + 1)

        rowLoop(freqTable, row, xIndex + 1)
      }
    }
  }

  let rec makeFreqTable = (freqTable: Map.t<int, int>, yIndex: int) => {
    switch yIndex === yLength {
    | true => freqTable
    | false => {
        let row = switch grid->Array.at(yIndex) {
        | None => []
        | Some(arr) => arr
        }

        makeFreqTable(rowLoop(freqTable, row, 0), yIndex + 1)
      }
    }
  }

  let freqTable = makeFreqTable(Map.make(), 0)

  let entries = freqTable->Map.entries->Array.fromIterator
  let (duplicate, numSet) = entries->Array.reduce((Int32.min_int, Set.make()), (acc, entry) => {
    let (duplicate, numSet) = acc
    let (num, freq) = entry
    numSet->Set.add(num)
    freq > 1 ? (num, numSet) : (duplicate, numSet)
  })

  let rec findMissing = (missing: int, numSet, end) => {
    switch end === 0 || missing !== 0 {
    | true => missing
    | false => findMissing(numSet->Set.has(end) ? missing : end, numSet, end - 1)
    }
  }
  let maxNum = xLength * yLength
  let missing = findMissing(0, numSet, maxNum)

  [duplicate, missing]
}

let g1 = [[9, 1, 7], [8, 9, 2], [3, 4, 6]]
let r1 = findMissingAndRepeatedValues(g1)
Console.log2("r1: ", r1)

let g2 = [[1, 3], [2, 2]]
let r2 = findMissingAndRepeatedValues(g2)
Console.log2("r2: ", r2)
