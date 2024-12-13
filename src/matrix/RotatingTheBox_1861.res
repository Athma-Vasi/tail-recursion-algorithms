// INCOMPLETE

type boxItem = | @as("#") Stone | @as("*") Obstacle | @as(".") Empty

let rotatingTheBox = (boxGrid: array<array<boxItem>>) => {
  let maxRows = Array.length(boxGrid)
  let row = switch boxGrid->Array.at(0) {
  | None => []
  | Some(r) => r
  }
  let maxColumns = Array.length(row)

  let transposeMatrix = (matrix: array<array<boxItem>>): array<array<boxItem>> => {
    let rec rowLoop = (transposedMatrix: array<array<boxItem>>, rowIndex: int) => {
      switch rowIndex === maxRows {
      | true => transposedMatrix
      | false => {
          let row = switch matrix->Array.at(rowIndex) {
          | None => []
          | Some(arr) => arr
          }

          let rec columnLoop = (colUpdatedMatrix: array<array<boxItem>>, columnIndex: int) => {
            switch columnIndex === maxColumns {
            | true => colUpdatedMatrix
            | false => {
                let matrixItem = switch row->Array.at(columnIndex) {
                | None => Empty
                | Some(item) => item
                }

                let rowToUpdate = switch colUpdatedMatrix->Array.at(columnIndex) {
                | None => []
                | Some(arr) => arr
                }

                let updatedRow =
                  rowToUpdate->Array.mapWithIndex((item, idx) =>
                    idx === rowIndex ? matrixItem : item
                  )

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

    let makeMatrix = (maxRows: int, maxColumns: int): array<array<boxItem>> => {
      let rec loop = (matrix: array<array<boxItem>>, rowCounter: int) => {
        rowCounter === maxRows
          ? matrix
          : loop(matrix->Array.concat([Array.make(~length=maxColumns, Empty)]), rowCounter + 1)
      }

      loop([], 0)
    }

    rowLoop(makeMatrix(maxRows, maxColumns), 0)
  }

  let reverseRows = (matrix: array<array<boxItem>>) => {
    let rec loop = (reversed: array<array<boxItem>>, index: int) => {
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

  let rotateBox = (boxGrid: array<array<boxItem>>) => {
    let transposed = transposeMatrix(boxGrid)
    reverseRows(transposed)
  }

  let applyGravitationalForce = (boxGrid: array<array<boxItem>>) => {
    let rec columnLoop = (applied: array<array<boxItem>>, columnIndex: int) => {
      switch columnIndex === maxColumns {
      | true => applied
      | false => {
          let rec rowLoop = (
            applied_: array<array<boxItem>>,
            anchorIndex: int,
            explorerIndex: int,
          ) => {
            switch explorerIndex < 0 {
            | true => applied_
            | false => {
                // let anchor = switch boxGrid->Array.at(anchorIndex) {
                // | None => Empty
                // | Some(arr) =>
                //   switch arr->Array.at(anchorIndex) {
                //   | None => Empty
                //   | Some(i) => i
                //   }
                // }

                let anchor =
                  boxGrid
                  ->Array.at(anchorIndex)
                  ->Option.flatMap(arr => arr->Array.at(anchorIndex))
                  ->Option.getOr(Empty)

                let explorer =
                  boxGrid
                  ->Array.at(explorerIndex)
                  ->Option.flatMap(arr => arr->Array.at(explorerIndex))
                  ->Option.getOr(Empty)

                switch (anchor, explorer) {
                | (Empty, Empty) => rowLoop(applied_, anchorIndex - 1, explorerIndex - 1)
                | (Empty, Obstacle) => rowLoop(applied_, anchorIndex - 1, explorerIndex - 1)
                | (Empty, Stone) => {
                    let anchorRow = switch applied_->Array.at(anchorIndex) {
                    | None => []
                    | Some(arr) => arr
                    }

                    let explorerRow = switch applied_->Array.at(explorerIndex) {
                    | None => []
                    | Some(arr) => arr
                    }

                    let updatedAnchorRow =
                      anchorRow->Array.mapWithIndex((item, idx) =>
                        idx === columnIndex ? Stone : item
                      )

                    let updatedExplorerRow =
                      explorerRow->Array.mapWithIndex((item, idx) =>
                        idx === columnIndex ? Empty : item
                      )

                    let updatedMatrix = applied_->Array.mapWithIndex((row, idx) =>
                      switch idx {
                      | idx if idx === anchorIndex => updatedAnchorRow
                      | idx if idx === explorerIndex => updatedExplorerRow
                      | _ => row
                      }
                    )

                    rowLoop(updatedMatrix, anchorIndex - 1, explorerIndex - 1)
                  }
                | (Obstacle, Empty) => rowLoop(applied_, anchorIndex, explorerIndex - 1)
                | (Obstacle, Obstacle) => rowLoop(applied_, anchorIndex - 1, explorerIndex - 1)
                | (Obstacle, Stone) => rowLoop(applied_, anchorIndex, explorerIndex - 1)
                | (Stone, Empty) => rowLoop(applied_, anchorIndex - 1, explorerIndex - 1)
                | (Stone, Obstacle) => rowLoop(applied_, anchorIndex - 1, explorerIndex - 1)
                | (Stone, Stone) => rowLoop(applied_, anchorIndex - 1, explorerIndex - 1)
                }
              }
            }
          }

          columnLoop(rowLoop(applied, maxRows - 1, maxRows - 2), columnIndex + 1)
        }
      }
    }

    columnLoop(boxGrid, 0)
  }

  rotateBox(boxGrid)->applyGravitationalForce
}

let b1 = [[Stone, Empty, Stone]]
let r1 = rotatingTheBox(b1)
Console.log2("r1: ", r1) // [[Empty], [Stone], [Stone]]
