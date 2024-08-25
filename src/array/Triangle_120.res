// incorrect

let minPathSum = (triangle: array<array<int>>) => {
  let rec climbSummit = (baseCamp: array<int>, index: int) => {
    switch index < 0 {
    | true => baseCamp
    | false => {
        let rec pullEquipUp = (tempBaseCamp: array<int>, innerIdx: int) => {
          switch innerIdx === Array.length(baseCamp) {
          | true => tempBaseCamp
          | false => {
              let person = switch baseCamp->Array.at(innerIdx) {
              | None => Int32.min_int
              | Some(n) => n
              }
              let below = switch triangle->Array.at(index + 1) {
              | None => []
              | Some(a) => a
              }
              let leftEquip = switch below->Array.at(innerIdx) {
              | None => Int32.min_int
              | Some(n) => n
              }
              let rightEquip = switch below->Array.at(innerIdx + 1) {
              | None => Int32.max_int
              | Some(n) => n
              }
              let reduce = person + (leftEquip < rightEquip ? leftEquip : rightEquip)
              let newTempBaseCamp = tempBaseCamp->Array.concat([reduce])

              Console.log(`\n`)
              Console.log("---pullEquipUp---")
              Console.log2("index: ", index)
              Console.log2("innerIdx: ", innerIdx)
              Console.log2("person: ", person)
              Console.log2("leftEquip: ", leftEquip)
              Console.log2("rightEquip: ", rightEquip)
              Console.log2("reduce: ", reduce)
              Console.log2("newTempBaseCamp: ", newTempBaseCamp)

              pullEquipUp(newTempBaseCamp, innerIdx + 1)
            }
          }
        }

        let newBaseCamp = pullEquipUp([], 0)

        Console.log(`\n`)
        Console.log("---climbSummit---")
        Console.log2("index: ", index)
        Console.log2("baseCamp: ", baseCamp)
        Console.log2("newBaseCamp: ", newBaseCamp)

        climbSummit(newBaseCamp, index - 1)
      }
    }
  }

  let startIdx = Array.length(triangle) - 2
  let start = switch triangle->Array.at(startIdx) {
  | None => []
  | Some(a) => a
  }

  Console.log(`\n`)
  Console.log("---minPathSum---")
  Console.log2("startIdx: ", startIdx)
  Console.log2("start: ", start)

  climbSummit(start, startIdx)
}

let t4 = [[1], [2, 3], [4, 5, 6]]
let r4 = minPathSum(t4)
Console.log2("r4: ", r4) // 7

// let t1 = [[2], [3, 4], [6, 5, 7], [4, 1, 8, 3]]
// let r1 = minPathSum(t1)
// Console.log2("r1: ", r1) // 11

// let t2 = [[-10]]
// let r2 = minPathSum(t2)
// Console.log2("r2: ", r2) // -10

// let t3 = [[-1], [2, 3], [1, -1, -3]]
// let r3 = minPathSum(t3)
// Console.log2("r3: ", r3) // -1
