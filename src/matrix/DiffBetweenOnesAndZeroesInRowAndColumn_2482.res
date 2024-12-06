// T(n) = O(n * m) where n is the number of rows and m is the number of columns
// S(n) = O(n + m)

let diffBetweenOnesAndZeroesInRowAndColumn = (grid: array<array<int>>) => {
  let maxRows = Array.length(grid)
  let row = switch grid->Array.at(0) {
  | None => []
  | Some(r) => r
  }
  let maxColumns = Array.length(row)

  let countZeroesAndOnes = row =>
    row->Array.reduce((0, 0), (acc, num) => {
      let (zeroesCount, onesCount) = acc
      num === 0 ? (zeroesCount + 1, onesCount) : (zeroesCount, onesCount + 1)
    })

  let rec makeBinaryCountTableRow = (rowTable: Map.t<int, (int, int)>, rowIndex: int) => {
    switch rowIndex === maxRows {
    | true => rowTable
    | false => {
        let row = switch grid->Array.at(rowIndex) {
        | None => []
        | Some(r) => r
        }

        let (zeroesCount, onesCount) = countZeroesAndOnes(row)
        rowTable->Map.set(rowIndex, (zeroesCount, onesCount))

        makeBinaryCountTableRow(rowTable, rowIndex + 1)
      }
    }
  }

  let rec makeBinaryCountTableColumn = (columnTable: Map.t<int, (int, int)>, columnIndex: int) => {
    switch columnIndex === maxColumns {
    | true => columnTable
    | false => {
        let rec collectBinaryColumn = (binaryColumn: array<int>, rowIndex: int) => {
          switch rowIndex === maxRows {
          | true => binaryColumn
          | false => {
              let row = switch grid->Array.at(rowIndex) {
              | None => []
              | Some(r) => r
              }
              let binary = switch row->Array.at(columnIndex) {
              | None => -1
              | Some(h) => h
              }

              collectBinaryColumn(binaryColumn->Array.concat([binary]), rowIndex + 1)
            }
          }
        }

        let binaryColumn = collectBinaryColumn([], 0)
        let (zeroesCount, onesCount) = countZeroesAndOnes(binaryColumn)
        columnTable->Map.set(columnIndex, (zeroesCount, onesCount))

        makeBinaryCountTableColumn(columnTable, columnIndex + 1)
      }
    }
  }

  let makeMatrix = (numberOfRows: int, numberOfColumns: int): array<array<int>> => {
    let rec loop = (matrix: array<array<int>>, rowCounter: int) => {
      rowCounter === numberOfRows
        ? matrix
        : loop(
            matrix->Array.concat([Array.make(~length=numberOfColumns, Int32.min_int)]),
            rowCounter + 1,
          )
    }

    loop([], 0)
  }

  let rowTable = makeBinaryCountTableRow(Map.make(), 0)
  let columnTable = makeBinaryCountTableColumn(Map.make(), 0)

  let rec makeDifferenceMatrix = (diffMatrix: array<array<int>>, rowIndex: int) => {
    switch rowIndex === maxRows {
    | true => diffMatrix
    | false => {
        let (zeroesCountRow, onesCountRow) = switch rowTable->Map.get(rowIndex) {
        | None => (0, 0)
        | Some(t) => t
        }

        let rec columnLoop = (columnIndex: int) => {
          switch columnIndex === maxColumns {
          | true => ()
          | false => {
              let (zeroesCountColumn, onesCountColumn) = switch columnTable->Map.get(columnIndex) {
              | None => (0, 0)
              | Some(t) => t
              }

              let diff = onesCountRow + onesCountColumn - zeroesCountRow - zeroesCountColumn
              let diffMatrixRow = switch diffMatrix->Array.at(rowIndex) {
              | None => []
              | Some(row) => row->Array.mapWithIndex((num, idx) => idx === columnIndex ? diff : num)
              }
              diffMatrix->Array.set(rowIndex, diffMatrixRow)

              columnLoop(columnIndex + 1)
            }
          }
        }

        columnLoop(0)
        makeDifferenceMatrix(diffMatrix, rowIndex + 1)
      }
    }
  }

  let diffMatrix = makeMatrix(maxRows, maxColumns)
  makeDifferenceMatrix(diffMatrix, 0)
}

let g1 = [[0, 1, 1], [1, 0, 1], [0, 0, 1]]
let r1 = diffBetweenOnesAndZeroesInRowAndColumn(g1)
Console.log2("r1: ", r1) // [[0,0,4],[0,0,4],[-2,-2,2]]

let g2 = [[1, 1, 1], [1, 1, 1]]
let r2 = diffBetweenOnesAndZeroesInRowAndColumn(g2)
Console.log2("r2: ", r2) // [[5,5,5],[5,5,5]]
