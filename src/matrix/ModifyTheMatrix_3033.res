// T(n) = O(m * n)
// S(n) = O(m * n)

let modifyTheMatrix = matrix => {
  let row = switch matrix->Array.at(0) {
  | None => []
  | Some(arr) => arr
  }
  let xLength = Array.length(row)
  let yLength = Array.length(matrix)

  let rec columnLoop = (answer: array<array<int>>, xIndex: int) => {
    switch xIndex === xLength {
    | true => answer
    | false => {
        let rec findMaxInColumn = (max: int, yIndex: int) => {
          switch yIndex === yLength {
          | true => max
          | false => {
              let row = switch matrix->Array.at(yIndex) {
              | None => []
              | Some(arr) => arr
              }
              let num = switch row->Array.at(xIndex) {
              | None => Int32.min_int
              | Some(n) => n
              }

              findMaxInColumn(max > num ? max : num, yIndex + 1)
            }
          }
        }

        let max = findMaxInColumn(Int32.min_int, 0)

        let rec replaceMaxInColumn = (replaced: array<array<int>>, yIndex: int) => {
          switch yIndex === yLength {
          | true => replaced
          | false => {
              let row = switch matrix->Array.at(yIndex) {
              | None => []
              | Some(arr) => arr
              }
              let num = switch row->Array.at(xIndex) {
              | None => Int32.min_int
              | Some(n) => n
              }

              row->Array.set(xIndex, num === -1 ? max : num)
              replaced->Array.set(yIndex, row)

              replaceMaxInColumn(replaced, yIndex + 1)
            }
          }
        }

        columnLoop(replaceMaxInColumn(answer, 0), xIndex + 1)
      }
    }
  }

  columnLoop(Array.copy(matrix), 0)
}

let m1 = [[1, 2, -1], [4, -1, 6], [7, 8, 9]]
let r1 = modifyTheMatrix(m1)
Console.log2("r1: ", r1)

let m2 = [[3, -1], [5, 2]]
let r2 = modifyTheMatrix(m2)
Console.log2("r2: ", r2)
