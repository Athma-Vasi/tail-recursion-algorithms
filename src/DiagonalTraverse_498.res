type previousDirection =
  | @as("start") Start | @as("right") Right | @as("down") Down | @as("left") Left | @as("up") Up

let diagonalTraverse = (matrix: array<array<int>>): array<int> => {
  let rows = Array.length(matrix)
  let firstRow = switch matrix->Array.get(0) {
  | None => []
  | Some(row) => row
  }
  let columns = Array.length(firstRow)

  let rec loop = (accumulator: array<int>, ~y: int, ~x: int): array<int> => {
    let isIndexSumEven = Float.mod(y->Int.toFloat +. x->Int.toFloat, 2.0) == 0.0
    let currentRow = switch matrix->Array.get(y) {
    | None => []
    | Some(row) => row
    }
    let currentNum = switch currentRow->Array.get(x) {
    | None => Int32.min_int
    | Some(num) => num
    }
    let clone = accumulator->Array.concat([currentNum])

    y === rows - 1 && x === columns - 1
      ? clone
      : switch isIndexSumEven {
        // even index direction: top-right
        | true =>
          // if at right wall
          switch x === columns - 1 {
          | true => loop(clone, ~y=y + 1, ~x) // go below
          | false =>
            // if at top ceiling
            switch y === 0 {
            | true => loop(clone, ~y, ~x=x + 1) // go right
            | false => loop(clone, ~y=y - 1, ~x=x + 1) // go top-right
            }
          }

        // odd index direction: bottom-left
        | false =>
          // if at left wall
          switch x === 0 {
          | true =>
            // if at bottom-left corner
            switch y === rows - 1 {
            | true => loop(clone, ~y, ~x=x + 1) // go right
            | false => loop(clone, ~y=y + 1, ~x) // go below
            }

          | false =>
            // if at bottom floor
            switch y === rows - 1 {
            | true => loop(clone, ~y, ~x=x + 1) // go right
            | false => loop(clone, ~y=y + 1, ~x=x - 1) // go bottom-left
            }
          }
        }
  }

  loop([], ~y=0, ~x=0)
}

let matrix1 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
let result1 = diagonalTraverse(matrix1)
Console.log2("matrix1", result1)

let matrix2 = [[1, 2], [3, 4]]
let result2 = diagonalTraverse(matrix2)
Console.log2("matrix2", result2)
