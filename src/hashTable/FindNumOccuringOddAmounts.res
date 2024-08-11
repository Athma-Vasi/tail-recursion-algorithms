// T(n) = O(n)
// S(n) = O(n)

let findNumOccuringOddAmounts = (nums: array<int>) => {
  let rec makeFreqTable = (freqTable: Map.t<int, int>, index: int) => {
    switch index === Array.length(nums) {
    | true => freqTable
    | false => {
        let num = switch nums->Array.at(index) {
        | None => Int32.min_int
        | Some(n) => n
        }
        let existingFreq = switch freqTable->Map.get(num) {
        | None => 0
        | Some(f) => f
        }
        freqTable->Map.set(num, existingFreq + 1)

        makeFreqTable(freqTable, index + 1)
      }
    }
  }

  let freqTable = makeFreqTable(Map.make(), 0)
  let tuples = freqTable->Map.entries->Array.fromIterator

  let rec loop = (foundNum: int, index: int) => {
    let (num, freq) = switch tuples->Array.at(index) {
    | None => (-1, -1)
    | Some(arr) => arr
    }
    let isNumFreqEven = Float.mod(Int.toFloat(freq), 2.0) == 0.0

    index === Array.length(tuples)
      ? foundNum
      : isNumFreqEven
      ? loop(foundNum, index + 1)
      : loop(num, index + 1)
  }

  loop(-1, 0)
}

let n1 = [1, 2, 3, 2, 3, 1, 3]
let r1 = findNumOccuringOddAmounts(n1)
Console.log2("r1: ", r1) // 3

let n2 = [5, 7, 2, 7, 5, 2, 5]
let r2 = findNumOccuringOddAmounts(n2)
Console.log2("r2: ", r2) // 5
