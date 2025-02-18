// let letterTilePossibilities = (tiles: string) => {
//   let rec makeFreqTable = (freqTable: Map.t<string, int>, tiles, index: int) => {
//     switch index === String.length(tiles) {
//     | true => freqTable
//     | false => {
//         let char = tiles->String.charAt(index)
//         let freq = freqTable->Map.get(char)->Option.mapOr(1, f => f + 1)
//         freqTable->Map.set(char, freq)

//         makeFreqTable(freqTable, tiles, index + 1)
//       }
//     }
//   }

//   let freqTable = makeFreqTable(Map.make(), tiles, 0)
//   freqTable
// }

// T(n) = O(n^2)
// S(n) = O(2^n)
// solution is not correct because it only works for n <= 3
let letterTilePossibilities = (tiles: string) => {
  let length = String.length(tiles)

  let rec outerLoop = (possibilities: Set.t<string>, outerIndex: int) => {
    switch outerIndex === length {
    | true => possibilities->Set.size
    | false => {
        let outerChar = tiles->String.charAt(outerIndex)
        possibilities->Set.add(outerChar)

        let rec cyclicRightLoop = (stack: string, cyclicIndex: int) => {
          switch cyclicIndex === outerIndex {
          | true => stack
          | false => {
              let cyclicChar = tiles->String.charAt(cyclicIndex)
              let newStack = stack->String.concat(cyclicChar)
              possibilities->Set.add(newStack)
              let newIndex =
                Float.mod(Int.toFloat(cyclicIndex + 1), Int.toFloat(length))->Float.toInt

              cyclicRightLoop(newStack, newIndex)
            }
          }
        }

        let rec cyclicLeftLoop = (stack: string, cyclicIndex: int) => {
          switch cyclicIndex === outerIndex {
          | true => stack
          | false => {
              let cyclicChar = tiles->String.charAt(cyclicIndex)
              let newStack = stack->String.concat(cyclicChar)
              possibilities->Set.add(newStack)
              let newIndex =
                Float.mod(Int.toFloat(cyclicIndex - 1), Int.toFloat(length))->Float.toInt

              cyclicLeftLoop(newStack, newIndex < 0 ? length - 1 : newIndex)
            }
          }
        }

        let stack = cyclicRightLoop(outerChar, outerIndex + 1 === length ? 0 : outerIndex + 1)
        possibilities->Set.add(stack)
        let stack = cyclicLeftLoop(outerChar, outerIndex - 1 < 0 ? length - 1 : outerIndex - 1)
        possibilities->Set.add(stack)

        outerLoop(possibilities, outerIndex + 1)
      }
    }
  }

  outerLoop(Set.make(), 0)
}

let t1 = "AAB"
let r1 = letterTilePossibilities(t1)
Console.log2("r1: AAB ", r1) // 8

let t2 = "AAABBC"
let r2 = letterTilePossibilities(t2)
Console.log2("r2: AAABBC", r2) // 188

let t3 = "V"
let r3 = letterTilePossibilities(t3)
Console.log2("r3: V", r3) // 1

let t4 = "ABC"
let r4 = letterTilePossibilities(t4)
Console.log2("r4: ABC", r4) // 15
// a, b, c,
// ab, bc, ca, ac, ba, cb,
// abc, bca, cab, acb, bac, cba

let t5 = "ABB"
let r5 = letterTilePossibilities(t5)
Console.log2("r5: ABB", r5) // 8

let t6 = "AAA"
let r6 = letterTilePossibilities(t6)
Console.log2("r6: AAA", r6) // 3
