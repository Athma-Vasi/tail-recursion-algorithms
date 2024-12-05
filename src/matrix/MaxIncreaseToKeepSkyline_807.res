// T(n) = O(n * m) where n is the number of rows and m is the number of columns
// S(n) = O(n + m)

let maxIncreaseToKeepSkyline = (grid: array<array<int>>) => {
  let maxRows = Array.length(grid)
  let row = switch grid->Array.at(0) {
  | None => []
  | Some(r) => r
  }
  let maxColumns = Array.length(row)

  let findTallest = arr =>
    arr->Array.reduce(Int32.min_int, (acc, height) => acc > height ? acc : height)

  let rec makeTallestInEachRowTable = (rowsTable: Map.t<int, int>, rowIndex: int) => {
    switch rowIndex === maxRows {
    | true => rowsTable
    | false => {
        let row = switch grid->Array.at(rowIndex) {
        | None => []
        | Some(r) => r
        }
        let tallest = findTallest(row)
        rowsTable->Map.set(rowIndex, tallest)

        makeTallestInEachRowTable(rowsTable, rowIndex + 1)
      }
    }
  }

  let rec makeTallestInEachColumnTable = (columnsTable: Map.t<int, int>, columnIndex: int) => {
    switch columnIndex === maxColumns {
    | true => columnsTable
    | false => {
        let rec collectHeights = (columnHeights: array<int>, rowIndex: int) => {
          switch rowIndex === maxRows {
          | true => columnHeights
          | false => {
              let row = switch grid->Array.at(rowIndex) {
              | None => []
              | Some(r) => r
              }
              let height = switch row->Array.at(columnIndex) {
              | None => -1
              | Some(h) => h
              }

              collectHeights(columnHeights->Array.concat([height]), rowIndex + 1)
            }
          }
        }

        let columnHeights = collectHeights([], 0)
        let tallest = findTallest(columnHeights)
        columnsTable->Map.set(columnIndex, tallest)

        makeTallestInEachColumnTable(columnsTable, columnIndex + 1)
      }
    }
  }

  let rowsTable = makeTallestInEachRowTable(Map.make(), 0)
  let columnsTable = makeTallestInEachColumnTable(Map.make(), 0)

  let rec rowLoop = (rowIncrease: int, rowIndex: int) => {
    switch rowIndex === maxRows {
    | true => rowIncrease
    | false => {
        let row = switch grid->Array.at(rowIndex) {
        | None => []
        | Some(r) => r
        }

        let rec columnLoop = (columnIncrease: int, columnIndex: int) => {
          switch columnIndex === maxColumns {
          | true => columnIncrease
          | false => {
              let height = switch row->Array.at(columnIndex) {
              | None => -1
              | Some(h) => h
              }
              let tallestInRow = switch rowsTable->Map.get(rowIndex) {
              | None => -1
              | Some(h) => h
              }
              let tallestInColumn = switch columnsTable->Map.get(columnIndex) {
              | None => -1
              | Some(h) => h
              }

              let smaller = tallestInRow < tallestInColumn ? tallestInRow : tallestInColumn
              let diffToIncrease = smaller - height

              columnLoop(columnIncrease + diffToIncrease, columnIndex + 1)
            }
          }
        }

        let columnIncrease = columnLoop(rowIncrease, 0)
        rowLoop(columnIncrease, rowIndex + 1)
      }
    }
  }

  rowLoop(0, 0)
}

let g1 = [[3, 0, 8, 4], [2, 4, 5, 7], [9, 2, 6, 3], [0, 3, 1, 0]]
let r1 = maxIncreaseToKeepSkyline(g1)
Console.log2("r1: ", r1) // 35

let g2 = [[0, 0, 0], [0, 0, 0], [0, 0, 0]]
let r2 = maxIncreaseToKeepSkyline(g2)
Console.log2("r2: ", r2) // 0
