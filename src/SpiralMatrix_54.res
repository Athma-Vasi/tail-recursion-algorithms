// INCORRECT

type nextDirection = Right | Down | Left | Up

let spiralMatrix = (matrix: array<array<int>>) => {
  let rows = Array.length(matrix)
  let firstRow = switch matrix->Array.get(0) {
  | None => []
  | Some(row) => row
  }
  let columns = Array.length(firstRow)
  let total = rows * columns

  let rec loop = (
    accumulator: array<int>,
    ~count: int,
    ~nextDirection: nextDirection,
    ~newMatrix: array<array<int>>,
    ~x: int,
    ~y: int,
  ) => {
    let newRows = Array.length(newMatrix)
    let newRow = switch newMatrix->Array.get(y) {
    | None => []
    | Some(row) => row
    }
    let newColumns = Array.length(newRow)

    let currentNum = switch newRow->Array.get(x) {
    | None => Int32.min_int
    | Some(num) => num
    }
    let clone = accumulator->Array.concat([currentNum])

    // Console.log(`
    // `)
    // Console.group("spiralMatrix")
    // Console.log2("accumulator", accumulator)
    // Console.log2("count", count)
    // Console.log2("nextDirection", nextDirection)
    // Console.log2("newMatrix", newMatrix)
    // Console.log2("newColumns", newColumns)
    // Console.log2("newRows", newRows)
    // Console.log2("x", x)
    // Console.log2("y", y)
    // Console.groupEnd()

    switch count === total {
    // if at ending position
    | true => clone
    | false =>
      switch x === count {
      // if at starting position
      | true => loop(clone, ~count=count + 1, ~newMatrix, ~nextDirection=Right, ~x=x + 1, ~y)
      | false =>
        switch x === newColumns - 1 {
        // if at right boundary
        | true => {
            let slicedMatrix = newMatrix->Array.slice(~start=y + 1, ~end=rows)
            let newY = y - 1 < 0 ? 0 : y - 1

            Console.log(`
              `)
            Console.group("right boundary")
            Console.log2("slicedMatrix", slicedMatrix)
            Console.log2("newY", newY)
            Console.groupEnd()

            loop(clone, ~count=count + 1, ~newMatrix=slicedMatrix, ~nextDirection=Down, ~x, ~y=newY)
          }
        | false =>
          switch y === newRows - 1 {
          // if at bottom boundary
          | true => {
              let newX = x - 1 < 0 ? 0 : x - 1

              let slicedMatrix = Array.reduceRight(newMatrix, [], (matrixAcc, currRow) => {
                let slicedRow = Array.reduceRightWithIndex(currRow, [], (
                  rowAcc,
                  num,
                  columnIndex,
                ) =>
                  switch columnIndex === newX {
                  | true => rowAcc
                  | false => rowAcc->Array.concat([num])
                  }
                )

                matrixAcc->Array.push(slicedRow)
                matrixAcc
              })

              loop(
                clone,
                ~count=count + 1,
                ~newMatrix=slicedMatrix,
                ~nextDirection=Left,
                ~x=newX,
                ~y,
              )
            }
          | false =>
            switch x === 0 {
            // if at left boundary
            | true => {
                let newMatrix = matrix->Array.slice(~start=0, ~end=rows - 1)
                let newY = y - 1 < 0 ? 0 : y - 1
                loop(clone, ~count=count + 1, ~newMatrix, ~nextDirection=Up, ~x, ~y=newY)
              }
            | false =>
              switch y === 0 {
              // if at top boundary
              | true => {
                  let newX = x - 1 < 0 ? 0 : x - 1

                  let slicedMatrix = Array.reduceRight(newMatrix, [], (matrixAcc, currRow) => {
                    let slicedRow = Array.reduceRightWithIndex(currRow, [], (
                      rowAcc,
                      num,
                      columnIndex,
                    ) =>
                      switch columnIndex === 0 {
                      | true => rowAcc
                      | false => rowAcc->Array.concat([num])
                      }
                    )

                    matrixAcc->Array.push(slicedRow)
                    matrixAcc
                  })

                  loop(
                    clone,
                    ~count=count + 1,
                    ~newMatrix=slicedMatrix,
                    ~nextDirection=Right,
                    ~x=newX,
                    ~y,
                  )
                }
              | false =>
                // if not at any boundary
                switch nextDirection {
                | Right =>
                  loop(
                    clone,
                    ~count=count + 1,
                    ~newMatrix,
                    ~nextDirection,
                    ~x=x + 1 === newColumns ? x : x + 1, // keep going right
                    ~y,
                  )
                | Down =>
                  loop(
                    clone,
                    ~count=count + 1,
                    ~newMatrix,
                    ~nextDirection,
                    ~x,
                    ~y=y + 1 === newRows ? y : y + 1, // keep going down
                  )
                | Left =>
                  loop(
                    clone,
                    ~count=count + 1,
                    ~newMatrix,
                    ~nextDirection,
                    ~x=x - 1 === 0 ? x : x - 1, // keep going left
                    ~y,
                  )
                | Up =>
                  loop(
                    clone,
                    ~count=count + 1,
                    ~newMatrix,
                    ~nextDirection,
                    ~x,
                    ~y=y - 1 === 0 ? y : y - 1, // keep going up
                  )
                }
              }
            }
          }
        }
      }
    }
  }

  loop([], ~count=0, ~newMatrix=matrix, ~nextDirection=Right, ~x=0, ~y=0)
}

let matrix1 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
let result1 = spiralMatrix(matrix1)
Console.log2("result1", result1)
