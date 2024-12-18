type direction = Up | Right | Down | Left

let numberOfIslandsDFS = (grid: array<array<int>>) => {
  let maxRows = Array.length(grid)
  let row = grid->Array.at(0)->Option.map(r => r)->Option.getOr([])
  let maxColumns = Array.length(row)

  // let directionCoordinates: Map.t<direction, (int, int)> = Map.make()
  // directionCoordinates->Map.set(Up, (-1, 0))
  // directionCoordinates->Map.set(Right, (0, 1))
  // directionCoordinates->Map.set(Down, (1, 0))
  // directionCoordinates->Map.set(Left, (0, -1))

  let rec depthFirstSearch = (
    matrix,
    direction,
    start: (int, int),
    current: (int, int),
    stop: bool,
  ) => {
    switch stop {
    | true => matrix
    | false => {
        let (startRowIndex, startColumnIndex) = start
        let (currRowIndex, currColumnIndex) = current
        let currCell =
          matrix
          ->Array.at(currRowIndex)
          ->Option.flatMap(row => row->Array.at(currColumnIndex))
          ->Option.getOr(0)

        Console.log("\n")
        Console.log("--depthFirstSearch--")
        Console.log2("matrix: ", matrix)
        Console.log2("direction: ", direction)
        Console.log2("start: ", start)
        Console.log2("current: ", current)
        Console.log2("stop: ", stop)

        switch direction {
        | Up =>
          switch currRowIndex < 0 {
          | true => depthFirstSearch(matrix, Right, start, start, stop)
          | false => {
              Console.log("\n")
              Console.log("--Up--")
              Console.log2("matrix: ", matrix)

              depthFirstSearch(matrix, direction, start, (currRowIndex - 1, currColumnIndex), stop)
            }
          }
        | Right =>
          switch currColumnIndex > maxColumns {
          | true => depthFirstSearch(matrix, Down, start, start, stop)
          | false => {
              Console.log("\n")
              Console.log("--Right--")
              Console.log2("matrix: ", matrix)

              depthFirstSearch(matrix, direction, start, (currRowIndex, currColumnIndex + 1), stop)
            }
          }
        | Down =>
          switch currRowIndex > maxRows {
          | true => depthFirstSearch(matrix, Left, start, start, stop)
          | false => {
              Console.log("\n")
              Console.log("--Down--")
              Console.log2("matrix: ", matrix)

              depthFirstSearch(matrix, direction, start, (currRowIndex + 1, currColumnIndex), stop)
            }
          }
        | Left =>
          switch currColumnIndex < 0 {
          | true => {
              // set starting cell to 0
              matrix
              ->Array.at(startRowIndex)
              ->Option.forEach(row => row->Array.set(startColumnIndex, 0))

              depthFirstSearch(matrix, Right, start, start, true) // stop
            }
          | false => {
              Console.log("\n")
              Console.log("--Left--")
              Console.log2("matrix: ", matrix)

              depthFirstSearch(matrix, direction, start, (currRowIndex, currColumnIndex - 1), stop)
            }
          }
        }
      }
    }
  }

  let rec rowLoop = (count: int, rowIndex: int) => {
    switch rowIndex === maxRows {
    | true => count
    | false => {
        let row = grid->Array.at(rowIndex)->Option.map(r => r)->Option.getOr([])

        Console.log("\n")
        Console.log("--rowLoop--")
        Console.log2("row: ", row)
        Console.log2("rowIndex: ", rowIndex)
        Console.log2("count: ", count)

        let rec columnLoop = (columnCount: int, modifiedGrid, columnIndex: int) => {
          switch columnIndex === maxColumns {
          | true => columnCount
          | false => {
              let cell = row->Array.at(columnIndex)->Option.map(c => c)->Option.getOr(0)
              let start = (rowIndex, columnIndex)

              Console.log("\n")
              Console.log("--columnLoop--")
              Console.log2("columnCount: ", columnCount)
              Console.log2("cell: ", cell)
              Console.log2("columnIndex: ", columnIndex)

              switch cell {
              | 1 => {
                  let modifiedGrid_ = depthFirstSearch(modifiedGrid, Up, start, start, false)
                  Console.log2("modifiedGrid_: ", modifiedGrid_)
                  columnLoop(columnCount + 1, modifiedGrid_, columnIndex + 1)
                }
              // 0
              | _ => columnLoop(columnCount, grid, columnIndex + 1)
              }
            }
          }
        }

        let columnCount = columnLoop(count, grid, 0)
        rowLoop(columnCount, rowIndex + 1)
      }
    }
  }

  rowLoop(0, 0)
}

let g1 = [[1, 1, 1, 1, 0], [1, 1, 0, 1, 0], [1, 1, 0, 0, 0], [0, 0, 0, 0, 0]]
let r1 = numberOfIslandsDFS(g1)
Console.log2("r1: ", r1) // 1

// --rowLoop--
// row:  [ 1, 1, 1, 1, 0 ]
// rowIndex:  0
// count:  0

// --columnLoop--
// columnCount:  0
// cell:  1
// columnIndex:  0

// --depthFirstSearch--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 0, 0 ]
// current:  [ 0, 0 ]
// stop:  false

// --Up--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 0, 0 ]
// current:  [ -1, 0 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 0, 0 ]
// current:  [ 0, 0 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 0, 0 ]
// current:  [ 0, 1 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 0, 0 ]
// current:  [ 0, 2 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 0, 0 ]
// current:  [ 0, 3 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 0, 0 ]
// current:  [ 0, 4 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 0, 0 ]
// current:  [ 0, 5 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 0, 0 ]
// current:  [ 0, 6 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 0 ]
// current:  [ 0, 0 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 0 ]
// current:  [ 1, 0 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 0 ]
// current:  [ 2, 0 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 0 ]
// current:  [ 3, 0 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 0 ]
// current:  [ 4, 0 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 0 ]
// current:  [ 5, 0 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 0, 0 ]
// current:  [ 0, 0 ]
// stop:  false

// --Left--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 1, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 0, 0 ]
// current:  [ 0, -1 ]
// stop:  false
// modifiedGrid_:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --columnLoop--
// columnCount:  1
// cell:  1
// columnIndex:  1

// --depthFirstSearch--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 0, 1 ]
// current:  [ 0, 1 ]
// stop:  false

// --Up--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 0, 1 ]
// current:  [ -1, 1 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 0, 1 ]
// current:  [ 0, 1 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 0, 1 ]
// current:  [ 0, 2 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 0, 1 ]
// current:  [ 0, 3 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 0, 1 ]
// current:  [ 0, 4 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 0, 1 ]
// current:  [ 0, 5 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 0, 1 ]
// current:  [ 0, 6 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 1 ]
// current:  [ 0, 1 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 1 ]
// current:  [ 1, 1 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 1 ]
// current:  [ 2, 1 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 1 ]
// current:  [ 3, 1 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 1 ]
// current:  [ 4, 1 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 1 ]
// current:  [ 5, 1 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 0, 1 ]
// current:  [ 0, 1 ]
// stop:  false

// --Left--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 0, 1 ]
// current:  [ 0, 0 ]
// stop:  false

// --Left--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 1, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 0, 1 ]
// current:  [ 0, -1 ]
// stop:  false
// modifiedGrid_:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --columnLoop--
// columnCount:  2
// cell:  1
// columnIndex:  2

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 0, 2 ]
// current:  [ 0, 2 ]
// stop:  false

// --Up--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 0, 2 ]
// current:  [ -1, 2 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 0, 2 ]
// current:  [ 0, 2 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 0, 2 ]
// current:  [ 0, 3 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 0, 2 ]
// current:  [ 0, 4 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 0, 2 ]
// current:  [ 0, 5 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 0, 2 ]
// current:  [ 0, 6 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 2 ]
// current:  [ 0, 2 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 2 ]
// current:  [ 1, 2 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 2 ]
// current:  [ 2, 2 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 2 ]
// current:  [ 3, 2 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 2 ]
// current:  [ 4, 2 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 2 ]
// current:  [ 5, 2 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 0, 2 ]
// current:  [ 0, 2 ]
// stop:  false

// --Left--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 0, 2 ]
// current:  [ 0, 1 ]
// stop:  false

// --Left--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 0, 2 ]
// current:  [ 0, 0 ]
// stop:  false

// --Left--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 1, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 0, 2 ]
// current:  [ 0, -1 ]
// stop:  false
// modifiedGrid_:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --columnLoop--
// columnCount:  3
// cell:  1
// columnIndex:  3

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 0, 3 ]
// current:  [ 0, 3 ]
// stop:  false

// --Up--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 0, 3 ]
// current:  [ -1, 3 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 0, 3 ]
// current:  [ 0, 3 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 0, 3 ]
// current:  [ 0, 4 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 0, 3 ]
// current:  [ 0, 5 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 0, 3 ]
// current:  [ 0, 6 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 3 ]
// current:  [ 0, 3 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 3 ]
// current:  [ 1, 3 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 3 ]
// current:  [ 2, 3 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 3 ]
// current:  [ 3, 3 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 3 ]
// current:  [ 4, 3 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 0, 3 ]
// current:  [ 5, 3 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 0, 3 ]
// current:  [ 0, 3 ]
// stop:  false

// --Left--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 0, 3 ]
// current:  [ 0, 2 ]
// stop:  false

// --Left--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 0, 3 ]
// current:  [ 0, 1 ]
// stop:  false

// --Left--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 0, 3 ]
// current:  [ 0, 0 ]
// stop:  false

// --Left--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 0, 3 ]
// current:  [ 0, -1 ]
// stop:  false
// modifiedGrid_:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --columnLoop--
// columnCount:  4
// cell:  0
// columnIndex:  4

// --rowLoop--
// row:  [ 1, 1, 0, 1, 0 ]
// rowIndex:  1
// count:  4

// --columnLoop--
// columnCount:  4
// cell:  1
// columnIndex:  0

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 1, 0 ]
// current:  [ 1, 0 ]
// stop:  false

// --Up--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 1, 0 ]
// current:  [ 0, 0 ]
// stop:  false

// --Up--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 1, 0 ]
// current:  [ -1, 0 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 1, 0 ]
// current:  [ 1, 0 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 1, 0 ]
// current:  [ 1, 1 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 1, 0 ]
// current:  [ 1, 2 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 1, 0 ]
// current:  [ 1, 3 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 1, 0 ]
// current:  [ 1, 4 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 1, 0 ]
// current:  [ 1, 5 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 1, 0 ]
// current:  [ 1, 6 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 1, 0 ]
// current:  [ 1, 0 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 1, 0 ]
// current:  [ 2, 0 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 1, 0 ]
// current:  [ 3, 0 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 1, 0 ]
// current:  [ 4, 0 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 1, 0 ]
// current:  [ 5, 0 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 1, 0 ]
// current:  [ 1, 0 ]
// stop:  false

// --Left--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 1, 0 ]
// current:  [ 1, -1 ]
// stop:  false
// modifiedGrid_:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --columnLoop--
// columnCount:  5
// cell:  1
// columnIndex:  1

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 1, 1 ]
// current:  [ 1, 1 ]
// stop:  false

// --Up--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 1, 1 ]
// current:  [ 0, 1 ]
// stop:  false

// --Up--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 1, 1 ]
// current:  [ -1, 1 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 1, 1 ]
// current:  [ 1, 1 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 1, 1 ]
// current:  [ 1, 2 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 1, 1 ]
// current:  [ 1, 3 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 1, 1 ]
// current:  [ 1, 4 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 1, 1 ]
// current:  [ 1, 5 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 1, 1 ]
// current:  [ 1, 6 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 1, 1 ]
// current:  [ 1, 1 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 1, 1 ]
// current:  [ 2, 1 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 1, 1 ]
// current:  [ 3, 1 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 1, 1 ]
// current:  [ 4, 1 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 1, 1 ]
// current:  [ 5, 1 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 1, 1 ]
// current:  [ 1, 1 ]
// stop:  false

// --Left--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 1, 1 ]
// current:  [ 1, 0 ]
// stop:  false

// --Left--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 1, 1 ]
// current:  [ 1, -1 ]
// stop:  false
// modifiedGrid_:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --columnLoop--
// columnCount:  6
// cell:  0
// columnIndex:  2

// --columnLoop--
// columnCount:  6
// cell:  1
// columnIndex:  3

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 1, 3 ]
// current:  [ 1, 3 ]
// stop:  false

// --Up--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 1, 3 ]
// current:  [ 0, 3 ]
// stop:  false

// --Up--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 1, 3 ]
// current:  [ -1, 3 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 1, 3 ]
// current:  [ 1, 3 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 1, 3 ]
// current:  [ 1, 4 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 1, 3 ]
// current:  [ 1, 5 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 1, 3 ]
// current:  [ 1, 6 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 1, 3 ]
// current:  [ 1, 3 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 1, 3 ]
// current:  [ 2, 3 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 1, 3 ]
// current:  [ 3, 3 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 1, 3 ]
// current:  [ 4, 3 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 1, 3 ]
// current:  [ 5, 3 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 1, 3 ]
// current:  [ 1, 3 ]
// stop:  false

// --Left--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 1, 3 ]
// current:  [ 1, 2 ]
// stop:  false

// --Left--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 1, 3 ]
// current:  [ 1, 1 ]
// stop:  false

// --Left--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 1, 3 ]
// current:  [ 1, 0 ]
// stop:  false

// --Left--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 1, 3 ]
// current:  [ 1, -1 ]
// stop:  false
// modifiedGrid_:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --columnLoop--
// columnCount:  7
// cell:  0
// columnIndex:  4

// --rowLoop--
// row:  [ 1, 1, 0, 0, 0 ]
// rowIndex:  2
// count:  7

// --columnLoop--
// columnCount:  7
// cell:  1
// columnIndex:  0

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 2, 0 ]
// current:  [ 2, 0 ]
// stop:  false

// --Up--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 2, 0 ]
// current:  [ 1, 0 ]
// stop:  false

// --Up--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 2, 0 ]
// current:  [ 0, 0 ]
// stop:  false

// --Up--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 2, 0 ]
// current:  [ -1, 0 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 2, 0 ]
// current:  [ 2, 0 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 2, 0 ]
// current:  [ 2, 1 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 2, 0 ]
// current:  [ 2, 2 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 2, 0 ]
// current:  [ 2, 3 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 2, 0 ]
// current:  [ 2, 4 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 2, 0 ]
// current:  [ 2, 5 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 2, 0 ]
// current:  [ 2, 6 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 2, 0 ]
// current:  [ 2, 0 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 2, 0 ]
// current:  [ 3, 0 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 2, 0 ]
// current:  [ 4, 0 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 2, 0 ]
// current:  [ 5, 0 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 2, 0 ]
// current:  [ 2, 0 ]
// stop:  false

// --Left--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 2, 0 ]
// current:  [ 2, -1 ]
// stop:  false
// modifiedGrid_:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --columnLoop--
// columnCount:  8
// cell:  1
// columnIndex:  1

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 2, 1 ]
// current:  [ 2, 1 ]
// stop:  false

// --Up--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 2, 1 ]
// current:  [ 1, 1 ]
// stop:  false

// --Up--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 2, 1 ]
// current:  [ 0, 1 ]
// stop:  false

// --Up--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Up
// start:  [ 2, 1 ]
// current:  [ -1, 1 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 2, 1 ]
// current:  [ 2, 1 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 2, 1 ]
// current:  [ 2, 2 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 2, 1 ]
// current:  [ 2, 3 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 2, 1 ]
// current:  [ 2, 4 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 2, 1 ]
// current:  [ 2, 5 ]
// stop:  false

// --Right--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Right
// start:  [ 2, 1 ]
// current:  [ 2, 6 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 2, 1 ]
// current:  [ 2, 1 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 2, 1 ]
// current:  [ 3, 1 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 2, 1 ]
// current:  [ 4, 1 ]
// stop:  false

// --Down--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Down
// start:  [ 2, 1 ]
// current:  [ 5, 1 ]
// stop:  false

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 2, 1 ]
// current:  [ 2, 1 ]
// stop:  false

// --Left--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 2, 1 ]
// current:  [ 2, 0 ]
// stop:  false

// --Left--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --depthFirstSearch--
// matrix:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]
// direction:  Left
// start:  [ 2, 1 ]
// current:  [ 2, -1 ]
// stop:  false
// modifiedGrid_:  [
//   [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0,
//     0
//   ]
// ]

// --columnLoop--
// columnCount:  9
// cell:  0
// columnIndex:  2

// --columnLoop--
// columnCount:  9
// cell:  0
// columnIndex:  3

// --columnLoop--
// columnCount:  9
// cell:  0
// columnIndex:  4

// --rowLoop--
// row:  [ 0, 0, 0, 0, 0 ]
// rowIndex:  3
// count:  9

// --columnLoop--
// columnCount:  9
// cell:  0
// columnIndex:  0

// --columnLoop--
// columnCount:  9
// cell:  0
// columnIndex:  1

// --columnLoop--
// columnCount:  9
// cell:  0
// columnIndex:  2

// --columnLoop--
// columnCount:  9
// cell:  0
// columnIndex:  3

// --columnLoop--
// columnCount:  9
// cell:  0
// columnIndex:  4
// r1:  9
