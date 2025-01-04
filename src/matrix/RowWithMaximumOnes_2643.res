// T(n) = O(n)
// S(n) = O(n)

let rowWithMaximumOnes = (matrix: array<array<int>>) => {
  let countOnes = (row: array<int>) => {
    let rec count = (amount: int, index: int) => {
      switch index === Array.length(row) {
      | true => amount
      | false => {
          let num = row->Array.at(index)->Option.mapOr(2, n => n)

          count(num === 1 ? amount + 1 : amount, index + 1)
        }
      }
    }

    count(0, 0)
  }

  let rec rowLoop = (rowCountTable: Map.t<int, int>, rowIndex: int) => {
    switch rowIndex === Array.length(matrix) {
    | true => rowCountTable
    | false => {
        let row = matrix->Array.at(rowIndex)->Option.mapOr([], r => r)
        rowCountTable->Map.set(rowIndex, countOnes(row))

        rowLoop(rowCountTable, rowIndex + 1)
      }
    }
  }

  rowLoop(Map.make(), 0)
  ->Map.entries
  ->Array.fromIterator
  ->Array.toSorted(((k1, v1), (k2, v2)) => {
    v2 - v1 === 0 ? Int.compare(k1, k2) : Int.compare(v2, v1)
  })
  ->Array.at(0)
  ->Option.mapOr((-1, -1), ((rowIndex, count)) => (rowIndex, count))
}

let m1 = [[0, 1], [1, 0]]
let r1 = rowWithMaximumOnes(m1)
Console.log2("r1: ", r1) // [0, 1]

let m2 = [[0, 0, 0], [0, 1, 1]]
let r2 = rowWithMaximumOnes(m2)
Console.log2("r2: ", r2) // [1, 2]

let m3 = [[0, 0], [1, 1], [0, 0]]
let r3 = rowWithMaximumOnes(m3)
Console.log2("r3: ", r3) // [1, 2]
