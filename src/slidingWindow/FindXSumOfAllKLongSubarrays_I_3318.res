// T(n) = O(n)
// S(n) = O(n)

let findXSumOfAllKLongSubarrays = (nums: array<int>, k: int, x: int) => {
  let calculateSum = (freqTable: Map.t<int, int>) => {
    let sortedTuples =
      freqTable
      ->Map.entries
      ->Core__Array.fromIterator
      ->Array.toSorted(((a1, a2), (b1, b2)) => b2 === a2 ? float(b1 - a1) : float(b2 - a2))

    sortedTuples->Array.reduceWithIndex(0, (acc, tuple, index) => {
      switch index + 1 > x {
      | true => acc
      | false => {
          let (num, frequency) = tuple
          let acc_ = acc + num * frequency
          acc_
        }
      }
    })
  }

  let rec expandWindow = (result: array<int>, freqTable: Map.t<int, int>, width: int) => {
    switch width === k {
    | true => (result->Array.concat([calculateSum(freqTable)]), freqTable)
    | false => {
        let num = switch nums->Array.at(width) {
        | None => 0
        | Some(n) => n
        }
        let frequency = switch freqTable->Map.get(num) {
        | None => 0
        | Some(f) => f
        }
        freqTable->Map.set(num, frequency + 1)

        expandWindow(result, freqTable, width + 1)
      }
    }
  }

  let rec slideWindow = (result: array<int>, freqTable: Map.t<int, int>, left: int, right: int) => {
    switch right === Array.length(nums) {
    | true => result
    | false => {
        let leftNum = switch nums->Array.at(left) {
        | None => -1
        | Some(n) => n
        }
        let leftNumFreq = switch freqTable->Map.get(leftNum) {
        | None => -1
        | Some(f) => f
        }
        leftNumFreq === 1
          ? freqTable->Map.delete(leftNum)->ignore
          : freqTable->Map.set(leftNum, leftNumFreq - 1)

        let rightNum = switch nums->Array.at(right) {
        | None => -1
        | Some(n) => n
        }
        let rightNumFreq = switch freqTable->Map.get(rightNum) {
        | None => 0
        | Some(f) => f
        }
        freqTable->Map.set(rightNum, rightNumFreq + 1)

        slideWindow(result->Array.concat([calculateSum(freqTable)]), freqTable, left + 1, right + 1)
      }
    }
  }

  let (result, freqTable) = expandWindow([], Map.make(), 0)
  slideWindow(result, freqTable, 0, k)
}

let n1 = [1, 1, 2, 2, 3, 4, 2, 3]
let k1 = 6
let x1 = 2
let r1 = findXSumOfAllKLongSubarrays(n1, k1, x1)
Console.log2("r1: ", r1) // [6,10,12]

let n2 = [3, 8, 7, 8, 7, 5]
let k2 = 2
let x2 = 2
let r2 = findXSumOfAllKLongSubarrays(n2, k2, x2)
Console.log2("r2: ", r2) // [11,15,15,15,12]
