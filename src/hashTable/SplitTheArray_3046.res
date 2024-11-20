// T(n) = O(n)
// S(n) = O(n)

let splitTheArray = (nums: array<int>) => {
  let rec loop = (freqTable: Map.t<int, int>, index: int) => {
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

        loop(freqTable, index + 1)
      }
    }
  }

  loop(Map.make(), 0)
  ->Map.values
  ->Core__Iterator.toArray
  ->Array.reduce(true, (acc, count) => count > 2 ? false : acc)
}

let n1 = [1, 1, 2, 2, 3, 4]
let r1 = splitTheArray(n1)
Console.log2("r1: ", r1)

let n2 = [1, 1, 1, 1]
let r2 = splitTheArray(n2)
Console.log2("r2: ", r2)
