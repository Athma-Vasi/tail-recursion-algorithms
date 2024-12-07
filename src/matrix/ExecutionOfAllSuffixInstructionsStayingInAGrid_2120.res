// T(n) = O(n)
// S(n) = O(n)

type move = R | D | L | U

let runAllSuffixInstructionsStayingInAGrid = (n: int, startPos: (int, int), moves: array<move>) => {
  let rec runInstructions = (result: array<int>, slicedMoves: array<move>) => {
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
          | true => instructions
          | false => {
              let move = switch slicedMoves->Array.at(movesIndex) {
              | None => R
              | Some(m) => m
              }

              switch move {
              | R =>
                switch colIndex + 1 > n - 1 {
                | true => traverse(instructions, true, movesIndex, rowIndex, colIndex + 1)
                | false => traverse(instructions + 1, false, movesIndex + 1, rowIndex, colIndex + 1)
                }
              | D =>
                switch rowIndex + 1 > n - 1 {
                | true => traverse(instructions, true, movesIndex, rowIndex + 1, colIndex)
                | false => traverse(instructions + 1, false, movesIndex + 1, rowIndex + 1, colIndex)
                }
              | L =>
                switch colIndex - 1 < 0 {
                | true => traverse(instructions, true, movesIndex, rowIndex, colIndex - 1)
                | false => traverse(instructions + 1, false, movesIndex + 1, rowIndex, colIndex - 1)
                }
              | U =>
                switch rowIndex - 1 < 0 {
                | true => traverse(instructions, true, movesIndex, rowIndex - 1, colIndex)
                | false => traverse(instructions + 1, false, movesIndex + 1, rowIndex - 1, colIndex)
                }
              }
            }
          }
        }

        let (startRowIndex, startColIndex) = startPos
        let instructions = traverse(0, false, 0, startRowIndex, startColIndex)
        runInstructions(
          result->Array.concat([instructions]),
          slicedMoves->Array.sliceToEnd(~start=1),
        )
      }
    }
  }

  runInstructions([], moves)
}

let n1 = 3
let sp1 = (0, 1)
let m1 = [R, R, D, D, L, U]
let r1 = runAllSuffixInstructionsStayingInAGrid(n1, sp1, m1)
Console.log2("r1: ", r1) // [1, 5, 4, 3, 1, 0]

let n2 = 2
let sp2 = (1, 1)
let m2 = [L, U, R, D]
let r2 = runAllSuffixInstructionsStayingInAGrid(n2, sp2, m2)
Console.log2("r2: ", r2) // [4, 1, 0, 0]
