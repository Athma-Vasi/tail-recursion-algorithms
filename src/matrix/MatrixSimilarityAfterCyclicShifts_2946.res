// T(n) = O(m * n)
// S(n) = O(m * n)

let matrixSimilarityAfterCyclicShifts = (matrix: array<array<int>>, k: int) => {
  let rec cyclicLeftShift = (shifted: array<int>, first: int, row: array<int>, index: int) => {
    switch index === Array.length(row) {
    | true => shifted->Array.concat([first])
    | false => {
        let num = switch row->Array.at(index) {
        | None => 0
        | Some(n) => n
        }

        switch index === 0 {
        | true => cyclicLeftShift(shifted, num, row, index + 1)
        | false => cyclicLeftShift(shifted->Array.concat([num]), first, row, index + 1)
        }
      }
    }
  }

  let rec cyclicRightShift = (shifted: array<int>, last: int, row: array<int>, index: int) => {
    switch index === Array.length(row) {
    | true => [last]->Array.concat(shifted)
    | false => {
        let num = switch row->Array.at(index) {
        | None => 0
        | Some(n) => n
        }

        switch index === Array.length(row) - 1 {
        | true => cyclicRightShift(shifted, num, row, index + 1)
        | false => cyclicRightShift(shifted->Array.concat([num]), last, row, index + 1)
        }
      }
    }
  }

  let numberOfRows = Array.length(matrix)
  let row = switch matrix->Array.at(0) {
  | None => []
  | Some(arr) => arr
  }
  let numberOfColumns = Array.length(row)

  let areMatrixesEqual = (matrix1: array<array<int>>, matrix2: array<array<int>>): bool => {
    let rec rowLoop = (rowsEqualSet: Set.t<bool>, rowIndex: int) => {
      switch rowIndex === numberOfRows {
      | true => rowsEqualSet->Set.has(false) ? false : true
      | false => {
          let row1 = switch matrix1->Array.at(rowIndex) {
          | None => []
          | Some(arr) => arr
          }
          let row2 = switch matrix2->Array.at(rowIndex) {
          | None => []
          | Some(arr) => arr
          }

          let rec columnLoop = (
            columnsEqualSet: Set.t<bool>,
            columnIndex: int,
            row1: array<int>,
            row2: array<int>,
          ) => {
            switch columnIndex === numberOfColumns {
            | true => columnsEqualSet->Set.has(false) ? false : true
            | false => {
                let num1 = switch row1->Array.at(columnIndex) {
                | None => Int32.min_int
                | Some(num) => num
                }
                let num2 = switch row2->Array.at(columnIndex) {
                | None => Int32.min_int
                | Some(num) => num
                }
                columnsEqualSet->Set.add(num1 === num2)

                columnLoop(columnsEqualSet, columnIndex + 1, row1, row2)
              }
            }
          }

          let areColumnsEqual = columnLoop(Set.make(), 0, row1, row2)
          rowsEqualSet->Set.add(areColumnsEqual)

          rowLoop(rowsEqualSet, rowIndex + 1)
        }
      }
    }

    rowLoop(Set.make(), 0)
  }

  let rec cyclicShift = (shiftedMatrix: array<array<int>>, count: int, index: int) => {
    switch count === k {
    | true => shiftedMatrix
    | false => {
        let row = switch shiftedMatrix->Array.at(index) {
        | None => []
        | Some(r) => r
        }

        let (newIdx, newCount) =
          index + 1 >= Array.length(row) ? (0, count + 1) : (index + 1, count)
        let isIndexEven = Float.mod(Int.toFloat(index), 2.0) === 0.0

        let replaceRowWithShifted = (shifted, matrix, index) =>
          matrix->Array.mapWithIndex((row, idx) => idx === index ? shifted : row)

        switch isIndexEven {
        | true => {
            let leftShifted = cyclicLeftShift([], 0, row, 0)
            let replaced = replaceRowWithShifted(leftShifted, shiftedMatrix, index)

            cyclicShift(replaced, newCount, newIdx)
          }
        | false => {
            let rightShifted = cyclicRightShift([], 0, row, 0)
            let replaced = replaceRowWithShifted(rightShifted, shiftedMatrix, index)

            cyclicShift(replaced, newCount, newIdx)
          }
        }
      }
    }
  }

  areMatrixesEqual(matrix, cyclicShift(matrix, 0, 0))
}

let m1 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
let k1 = 4
let r1 = matrixSimilarityAfterCyclicShifts(m1, k1)
Console.log2("r1: ", r1) // false

let m2 = [[1, 2, 1, 2], [5, 5, 5, 5], [6, 3, 6, 3]]
let k2 = 2
let r2 = matrixSimilarityAfterCyclicShifts(m2, k2)
Console.log2("r2: ", r2) // true

let m3 = [[2, 2], [2, 2]]
let k3 = 3
let r3 = matrixSimilarityAfterCyclicShifts(m3, k3)
Console.log2("r3: ", r3) // true
