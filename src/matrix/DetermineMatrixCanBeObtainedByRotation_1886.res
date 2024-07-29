// T(n) = O(m * n)
// S(n) = O(m * n)

let determineMatrixCanBeObtainedByRotation = (
  matrix: array<array<int>>,
  target: array<array<int>>,
) => {
  // assume same dimensions for both
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

  let transposeMatrix = (matrix: array<array<int>>): array<array<int>> => {
    let rec rowLoop = (transposedMatrix: array<array<int>>, rowIndex: int) => {
      switch rowIndex === numberOfRows {
      | true => transposedMatrix
      | false => {
          let row = switch matrix->Array.at(rowIndex) {
          | None => []
          | Some(arr) => arr
          }

          let rec columnLoop = (colUpdatedMatrix: array<array<int>>, columnIndex: int) => {
            switch columnIndex === numberOfColumns {
            | true => colUpdatedMatrix
            | false => {
                let matrixNum = switch row->Array.at(columnIndex) {
                | None => Int32.min_int
                | Some(num) => num
                }

                let rowToUpdate = switch colUpdatedMatrix->Array.at(columnIndex) {
                | None => []
                | Some(arr) => arr
                }

                let updatedRow =
                  rowToUpdate->Array.mapWithIndex((num, idx) => idx === rowIndex ? matrixNum : num)

                let updatedMatrix =
                  colUpdatedMatrix->Array.mapWithIndex((row, idx) =>
                    idx === columnIndex ? updatedRow : row
                  )

                columnLoop(updatedMatrix, columnIndex + 1)
              }
            }
          }

          rowLoop(columnLoop(transposedMatrix, 0), rowIndex + 1)
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

    rowLoop(makeMatrix(numberOfRows, numberOfColumns), 0)
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

  let rec rotationLoop = (
    isObtainedSet: Set.t<bool>,
    counter: int,
    rotated: array<array<int>>,
    rotations: int,
  ) => {
    switch counter === rotations {
    | true => isObtainedSet->Set.has(true)
    | false => {
        let transposedMatrix = transposeMatrix(rotated)
        let rotatedMatrix = reverseRows(transposedMatrix)

        isObtainedSet->Set.add(areMatrixesEqual(rotatedMatrix, target))
        rotationLoop(isObtainedSet, counter + 1, rotatedMatrix, rotations)
      }
    }
  }

  rotationLoop(Set.make(), 0, matrix, 4) // 90, 180, 270, 0
}

let m1 = [[0, 1], [1, 0]]
let t1 = [[1, 0], [0, 1]]
let r1 = determineMatrixCanBeObtainedByRotation(m1, t1)
Console.log2("r1: ", r1)

let m2 = [[0, 1], [1, 1]]
let t2 = [[1, 0], [0, 1]]
let r2 = determineMatrixCanBeObtainedByRotation(m2, t2)
Console.log2("r2: ", r2)

let m3 = [[0, 0, 0], [0, 1, 0], [1, 1, 1]]
let t3 = [[1, 1, 1], [0, 1, 0], [0, 0, 0]]
let r3 = determineMatrixCanBeObtainedByRotation(m3, t3)
Console.log2("r3: ", r3)
