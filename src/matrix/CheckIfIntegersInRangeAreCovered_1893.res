// T(n) = O(m * n)
// S(n) = O(m * n)

let checkIfIntegersInRangeAreCovered = (ranges: array<array<int>>, left: int, right: int) => {
  let rowLength = Array.length(ranges)
  let column = switch ranges->Array.at(0) {
  | None => []
  | Some(arr) => arr
  }
  let columnLength = Array.length(column)

  let rec rowLoop = (rowCoveredSet: Set.t<bool>, rowIndex: int) => {
    switch rowIndex === rowLength {
    | true => rowCoveredSet
    | false => {
        let currentRow = switch ranges->Array.at(rowIndex) {
        | None => []
        | Some(arr) => arr
        }

        let rec columnLoop = (colCoveredSet: Set.t<bool>, columnIndex: int) => {
          switch columnIndex === columnLength {
          | true => colCoveredSet->Set.has(true)
          | false => {
              let currentNum = switch currentRow->Array.at(columnIndex) {
              | None => Int32.min_int
              | Some(num) => num
              }

              switch currentNum >= left && currentNum <= right {
              | true => {
                  colCoveredSet->Set.add(true)
                  columnLoop(colCoveredSet, columnIndex + 1)
                }
              | false => {
                  colCoveredSet->Set.add(false)
                  columnLoop(colCoveredSet, columnIndex + 1)
                }
              }
            }
          }
        }

        let isRowCovered = columnLoop(Set.make(), 0)
        rowCoveredSet->Set.add(isRowCovered)
        rowLoop(rowCoveredSet, rowIndex + 1)
      }
    }
  }

  rowLoop(Set.make(), 0)->Set.has(true)
}

let ranges1 = [[1, 2], [3, 4], [5, 6]]
let l1 = 2
let r1 = 5
let r1 = checkIfIntegersInRangeAreCovered(ranges1, l1, r1)
Console.log2("r1: ", r1)

let ranges2 = [[1, 10], [10, 20]]
let l2 = 21
let r2 = 21
let r2 = checkIfIntegersInRangeAreCovered(ranges2, l2, r2)
Console.log2("r2: ", r2)
