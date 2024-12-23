// T(n) = O(m * n)
// S(n) = O(m * n)

let transposeMatrix = (matrix: array<array<int>>, maxRows: int, maxColumns: int): array<
  array<int>,
> => {
  let rec transpose = (transposed: array<array<int>>, rowIndex: int) => {
    switch rowIndex === maxRows {
    | true => transposed
    | false => {
        let row = switch matrix->Array.at(rowIndex) {
        | None => []
        | Some(arr) => arr
        }

        let rec columnLoop = (columnUpdated: array<array<int>>, columnIndex: int) => {
          switch columnIndex === maxColumns {
          | true => columnUpdated
          | false => {
              let matrixItem = switch row->Array.at(columnIndex) {
              | None => 0
              | Some(item) => item
              }

              let rowToUpdate = switch columnUpdated->Array.at(columnIndex) {
              | None => []
              | Some(arr) => arr
              }

              let updatedRow =
                rowToUpdate->Array.mapWithIndex((item, idx) => idx === rowIndex ? matrixItem : item)

              let updatedMatrix =
                columnUpdated->Array.mapWithIndex((row, idx) =>
                  idx === columnIndex ? updatedRow : row
                )

              columnLoop(updatedMatrix, columnIndex + 1)
            }
          }
        }

        transpose(columnLoop(transposed, 0), rowIndex + 1)
      }
    }
  }

  let makeMatrix = (maxRows: int, maxColumns: int): array<array<int>> => {
    let rec loop = (matrix: array<array<int>>, rowCounter: int) => {
      rowCounter === maxRows
        ? matrix
        : loop(matrix->Array.concat([Array.make(~length=maxColumns, 0)]), rowCounter + 1)
    }

    loop([], 0)
  }

  transpose(makeMatrix(maxRows, maxColumns), 0)
}

let reverseRows = (matrix: array<array<int>>) => {
  let rec loop = (reversed: array<array<int>>, index: int) => {
    switch index === Array.length(matrix) {
    | true => reversed
    | false => {
        let row = switch matrix->Array.at(index) {
        | None => []
        | Some(arr) => arr
        }

        loop(reversed->Array.concat([Array.toReversed(row)]), index + 1)
      }
    }
  }

  loop([], 0)
}

let rotateImage = (matrix: array<array<int>>) => {
  let maxRows = Array.length(matrix)
  let row = switch matrix->Array.at(0) {
  | None => []
  | Some(r) => r
  }
  let maxColumns = Array.length(row)

  transposeMatrix(matrix, maxRows, maxColumns)->reverseRows
}

let m1 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
let r1 = rotateImage(m1)
Console.log2("r1: ", r1) // [[7, 4, 1], [8, 5, 2], [9, 6, 3]]

let m2 = [[5, 1, 9, 11], [2, 4, 8, 10], [13, 3, 6, 7], [15, 14, 12, 16]]
let r2 = rotateImage(m2)
Console.log2("r2: ", r2) // [[15, 13, 2, 5], [14, 3, 4, 1], [12, 6, 8, 9], [16, 7, 10, 11]]
