// INCOMPLETE

type move = R | D | L | U

let runAllSuffixInstructionsStayingInAGrid = (n: int, startPos: (int, int), moves: array<move>) => {
  let rec runInstructions = (
    result: array<int>,
    slicedMoves: array<move>,
    startRowIndex: int,
    startColIndex: int,
  ) => {
    let length = Array.length(slicedMoves)

    switch length === 0 {
    | true => result
    | false => {
        let rec traverse = (
          instructions: int,
          isOutOfBounds: bool,
          movesIndex: int,
          rowIndex: int,
          colIndex: int,
        ) => {
          switch movesIndex === length || isOutOfBounds {
          | true => (instructions, isOutOfBounds, rowIndex, colIndex)
          | false => {
              let move = switch slicedMoves->Array.at(movesIndex) {
              | None => R
              | Some(m) => m
              }

              Console.log("\n")
              Console.log("--traverse--")
              Console.log2("instructions: ", instructions)
              Console.log2("isOutOfBounds: ", isOutOfBounds)
              Console.log2("movesIndex: ", movesIndex)
              Console.log2("rowIndex: ", rowIndex)
              Console.log2("colIndex: ", colIndex)
              Console.log2("move: ", move)

              switch move {
              | R => {
                  let newColIndex = colIndex + 1

                  Console.log("--R--")
                  Console.log2("newColIndex: ", newColIndex)

                  switch newColIndex > n - 1 {
                  | true => traverse(instructions, true, movesIndex, rowIndex, newColIndex)
                  | false =>
                    traverse(instructions + 1, false, movesIndex + 1, rowIndex, newColIndex)
                  }
                }
              | D => {
                  let newRowIndex = rowIndex + 1

                  Console.log("--D--")
                  Console.log2("newRowIndex: ", newRowIndex)

                  switch newRowIndex > n - 1 {
                  | true => traverse(instructions, true, movesIndex, newRowIndex, colIndex)
                  | false =>
                    traverse(instructions + 1, false, movesIndex + 1, newRowIndex, colIndex)
                  }
                }
              | L => {
                  let newColIndex = colIndex - 1

                  Console.log("--L--")
                  Console.log2("newColIndex: ", newColIndex)

                  switch newColIndex < 0 {
                  | true => traverse(instructions, true, movesIndex, rowIndex, newColIndex)
                  | false =>
                    traverse(instructions + 1, false, movesIndex + 1, rowIndex, newColIndex)
                  }
                }
              | U => {
                  let newRowIndex = rowIndex - 1

                  Console.log("--U--")
                  Console.log2("newRowIndex: ", newRowIndex)

                  switch newRowIndex < 0 {
                  | true => traverse(instructions, true, movesIndex, newRowIndex, colIndex)
                  | false =>
                    traverse(instructions + 1, false, movesIndex + 1, newRowIndex, colIndex)
                  }
                }
              }
            }
          }
        }

        let (instructions, isOutOfBounds, rowIndex, colIndex) = traverse(
          0,
          false,
          0,
          startRowIndex,
          startColIndex,
        )
        Console.log("\n")
        Console.log("--outside traverse--")
        Console.log2("instructions: ", instructions)
        Console.log2("rowIndex: ", rowIndex)
        Console.log2("colIndex: ", colIndex)
        runInstructions(
          result->Array.concat([instructions]),
          slicedMoves->Array.sliceToEnd(~start=1),
          rowIndex,
          colIndex,
        )
      }
    }
  }

  let (rowStart, colStart) = startPos
  runInstructions([], moves, rowStart, colStart)
}

let n1 = 3
let sp1 = (0, 1)
let m1 = [R, R, D, D, L, U]
let r1 = runAllSuffixInstructionsStayingInAGrid(n1, sp1, m1)
Console.log2("r1: ", r1)
