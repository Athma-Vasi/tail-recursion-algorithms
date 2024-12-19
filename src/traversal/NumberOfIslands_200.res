type direction = Up | Right | Down | Left
type axis = Row | Column

let numberOfIslandsDFS = (grid: array<array<int>>) => {
  let maxRows = Array.length(grid)
  let row = grid->Array.at(0)->Option.map(r => r)->Option.getOr([])
  let maxColumns = Array.length(row)

  let rec depthFirstSearch = (
    visited: Set.t<string>,
    direction,
    start: (int, int),
    current: (int, int),
  ) => {
    let (startRowIndex, startColumnIndex) = start
    let startCell =
      grid
      ->Array.at(startRowIndex)
      ->Option.flatMap(row => row->Array.at(startColumnIndex))
      ->Option.getOr(0)

    switch startCell === 0 {
    | true => visited
    | false => {
        let (currRowIndex, currColumnIndex) = current
        let currCell =
          grid
          ->Array.at(currRowIndex)
          ->Option.flatMap(row => row->Array.at(currColumnIndex))
          ->Option.getOr(0)
        visited->Set.add(Int.toString(currRowIndex) ++ "," ++ Int.toString(currColumnIndex))

        Console.log("\n")
        Console.log("--depthFirstSearch--")
        Console.log2("direction: ", direction)
        Console.log2("start: ", start)
        Console.log2("current: ", current)
        Console.log2("visited: ", visited)

        switch direction {
        | Up =>
          switch currRowIndex < 0 || currCell === 0 {
          | true => depthFirstSearch(visited, Right, start, start)
          | false =>
            depthFirstSearch(visited, direction, start, (currRowIndex - 1, currColumnIndex))
          }
        | Right =>
          switch currColumnIndex > maxColumns || currCell === 0 {
          | true => depthFirstSearch(visited, Down, start, start)
          | false =>
            depthFirstSearch(visited, direction, start, (currRowIndex, currColumnIndex + 1))
          }
        | Down =>
          switch currRowIndex > maxRows || currCell === 0 {
          | true => depthFirstSearch(visited, Left, start, start)
          | false =>
            depthFirstSearch(visited, direction, start, (currRowIndex + 1, currColumnIndex))
          }
        | Left =>
          switch currColumnIndex < 0 || currCell === 0 {
          | true => depthFirstSearch(visited, Right, start, start)
          | false =>
            depthFirstSearch(visited, direction, start, (currRowIndex, currColumnIndex - 1))
          }
        }
      }
    }
  }

  let rec rowLoop = (count: int, rowVisited: Set.t<string>, rowIndex: int) => {
    switch rowIndex === maxRows {
    | true => count
    | false => {
        let row = grid->Array.at(rowIndex)->Option.map(r => r)->Option.getOr([])

        Console.log("\n")
        Console.log("--rowLoop--")
        Console.log2("row: ", row)
        Console.log2("rowIndex: ", rowIndex)
        Console.log2("count: ", count)
        Console.log2("rowVisited: ", rowVisited)

        let rec columnLoop = (columnCount: int, columnVisited: Set.t<string>, columnIndex: int) => {
          switch columnIndex === maxColumns {
          | true => (columnCount, columnVisited)
          | false => {
              let cell = row->Array.at(columnIndex)->Option.map(c => c)->Option.getOr(0)
              let start = (rowIndex, columnIndex)
              let isCellVisited =
                columnVisited->Set.has(Int.toString(rowIndex) ++ "," ++ Int.toString(columnIndex))
              // let isRowVisited =
              //   columnVisited
              //   ->Map.get(Row)
              //   ->Option.map(set => set->Set.has(rowIndex))
              //   ->Option.getOr(false)
              // let isColumnVisited =
              //   columnVisited
              //   ->Map.get(Column)
              //   ->Option.map(set => set->Set.has(columnIndex))
              //   ->Option.getOr(false)

              Console.log("\n")
              Console.log("--columnLoop--")
              Console.log2("columnCount: ", columnCount)
              Console.log2("cell: ", cell)
              Console.log2("columnIndex: ", columnIndex)
              Console.log2("columnVisited: ", columnVisited)
              Console.log2("isCellVisited: ", isCellVisited)

              switch cell === 0 || isCellVisited {
              | true => columnLoop(columnCount, columnVisited, columnIndex + 1)
              | false => {
                  let columnVisited_ = depthFirstSearch(columnVisited, Up, start, start)
                  columnLoop(columnCount + 1, columnVisited_, columnIndex + 1)
                }
              }
            }
          }
        }

        let (columnCount, columnVisited) = columnLoop(count, rowVisited, 0)
        rowLoop(columnCount, columnVisited, rowIndex + 1)
      }
    }
  }

  rowLoop(0, Set.make(), 0)
}

let g1 = [[1, 1, 1, 1, 0], [1, 1, 0, 1, 0], [1, 1, 0, 0, 0], [0, 0, 0, 0, 0]]
let r1 = numberOfIslandsDFS(g1)
Console.log2("r1: ", r1) // 1

// let g2 = [[1, 1, 0, 0, 0], [1, 1, 0, 0, 0], [0, 0, 1, 0, 0], [0, 0, 0, 1, 1]]
// let r2 = numberOfIslandsDFS(g2)
// Console.log2("r2: ", r2) // 3
