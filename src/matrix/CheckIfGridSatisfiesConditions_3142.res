// T(n) = O(m * n)
// S(n) = O(m * n)

let checkIfGridSatisfiesConditions = (grid: array<array<int>>) => {
  let rec checkColumnWiseInequalities = (inequalities: Set.t<bool>, yIndex: int) => {
    switch yIndex === Array.length(grid) {
    | true => inequalities
    | false => {
        let row = switch grid->Array.at(yIndex) {
        | None => []
        | Some(arr) => arr
        }

        let rec checkColumn = (columnInequalities: Set.t<bool>, stack: array<int>, xIndex: int) => {
          switch xIndex === Array.length(row) {
          | true => columnInequalities
          | false => {
              let currNum = switch row->Array.at(xIndex) {
              | None => -1
              | Some(n) => n
              }
              let prevNum = switch stack->Array.at(-1) {
              | None => -1
              | Some(n) => n
              }
              let updatedStack = stack->Array.concat([currNum])

              switch prevNum < 0 {
              | true => checkColumn(columnInequalities, updatedStack, xIndex + 1)
              | false => {
                  columnInequalities->Set.add(prevNum !== currNum)

                  checkColumn(columnInequalities, updatedStack, xIndex + 1)
                }
              }
            }
          }
        }

        let columnInequalities = switch Array.length(row) < 2 {
        | true => {
            let columnInequalities = Set.make()
            columnInequalities->Set.add(false)
            columnInequalities
          }
        | false => checkColumn(Set.make(), [], 0)
        }
        inequalities->Set.add(columnInequalities->Set.has(false) ? false : true)
        checkColumnWiseInequalities(inequalities, yIndex + 1)
      }
    }
  }

  let rec checkRowEqualities = (equalities: Set.t<bool>, yIndex: int) => {
    switch Array.length(grid) < 2 {
    | true => {
        let equalities = Set.make()
        equalities->Set.add(false)
        equalities
      }
    | false =>
      switch yIndex === Array.length(grid) {
      | true => equalities
      | false => {
          let row = switch grid->Array.at(yIndex) {
          | None => []
          | Some(arr) => arr
          }

          let rec checkRow = (
            rowEqualities: Set.t<bool>,
            stack: array<array<int>>,
            xIndex: int,
          ) => {
            switch xIndex === Array.length(row) {
            | true => equalities
            | false => {
                let currNum = switch row->Array.at(xIndex) {
                | None => -1
                | Some(n) => n
                }
                let prevRow = switch stack->Array.at(-1) {
                | None => []
                | Some(arr) => arr
                }
                let updatedStack = stack->Array.concat([row])

                switch Array.length(prevRow) === 0 {
                | true => checkRow(rowEqualities, updatedStack, xIndex + 1)
                | false => {
                    let prevNum = switch prevRow->Array.at(xIndex) {
                    | None => -1
                    | Some(n) => n
                    }
                    rowEqualities->Set.add(prevNum === currNum)

                    checkRow(rowEqualities, updatedStack, xIndex + 1)
                  }
                }
              }
            }
          }

          let rowEqualities = checkRow(Set.make(), [], 0)
          equalities->Set.add(rowEqualities->Set.has(false) ? false : true)
          checkRowEqualities(equalities, yIndex + 1)
        }
      }
    }
  }

  let columnInequality = checkColumnWiseInequalities(Set.make(), 0)->Set.has(false)
  let rowEquality = checkRowEqualities(Set.make(), 0)->Set.has(false)

  !columnInequality && !rowEquality
}

let g1 = [[1, 0, 2], [1, 0, 2]]
let r1 = checkIfGridSatisfiesConditions(g1)
Console.log2("r1: ", r1) // true

let g2 = [[1, 1, 1], [0, 0, 0]]
let r2 = checkIfGridSatisfiesConditions(g2)
Console.log2("r2: ", r2) // false

let g3 = [[1], [2], [3]]
let r3 = checkIfGridSatisfiesConditions(g3)
Console.log2("r3: ", r3) // false
