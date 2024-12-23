let checkIfArrayIsGood = (nums: array<int>) => {
  let rec makeFreqTable = (freqTable: Map.t<int, int>, index: int) => {
    switch index === Array.length(nums) {
    | true => freqTable
    | false => {
        let num = nums->Array.at(index)->Option.mapOr(0, n => n)
        let freq = freqTable->Map.get(num)->Option.mapOr(1, c => c + 1)
        freqTable->Map.set(num, freq)

        makeFreqTable(freqTable, index + 1)
      }
    }
  }

  let freqTable = makeFreqTable(Map.make(), 0)
  let length = Array.length(nums)
  let max = nums->Array.reduce(Int32.min_int, (acc, num) => acc > num ? acc : num)

  let rec check = (isGood: bool, index: int) => {
    switch index === length {
    | true => isGood
    | false => {
        let num = nums->Array.at(index)->Option.mapOr(0, n => n)
        let freq = freqTable->Map.get(num)->Option.mapOr(0, c => c)

        check((num === max && freq === 2) || (num !== max && freq === 1), index + 1)
      }
    }
  }

  let correctLength = max + 1
  length === correctLength && check(false, 0)
}

let n1 = [2, 1, 3]
let r1 = checkIfArrayIsGood(n1)
Console.log2("r1: ", r1)

let n2 = [1, 3, 3, 2]
let r2 = checkIfArrayIsGood(n2)
Console.log2("r2: ", r2)

let n3 = [1, 1]
let r3 = checkIfArrayIsGood(n3)
Console.log2("r3: ", r3)

let n4 = [3, 4, 4, 1, 2, 1]
let r4 = checkIfArrayIsGood(n4)
Console.log2("r4: ", r4)
