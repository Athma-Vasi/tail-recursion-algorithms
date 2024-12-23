// T(n) = O(m * n)
// S(n) = O(m + n)

let minNumberOfFlipsRequiredToMakeBinaryGridPalindromic_I = (grid: array<array<int>>) => {
  let countPalindromicFlips = (nums: array<int>) => {
    let rec count = (amount: int, sliced: array<int>) => {
      let length = Array.length(sliced)

      switch length < 2 {
      | true => amount
      | false => {
          let head = nums->Array.at(0)->Option.mapOr(-1, b => b)
          let tail = nums->Array.at(-1)->Option.mapOr(-1, b => b)

          count(head === tail ? amount : amount + 1, sliced->Array.slice(~start=1, ~end=length - 1))
        }
      }
    }

    count(0, nums)
  }

  let maxRows = Array.length(grid)
  let row = grid->Array.at(0)->Option.mapOr([], r => r)
  let maxColumns = Array.length(row)

  let rec rowLoop = (rowFlips: int, rowIndex: int) => {
    switch rowIndex === maxRows {
    | true => rowFlips
    | false => {
        let row = grid->Array.at(rowIndex)->Option.mapOr([], r => r)
        let count = countPalindromicFlips(row)

        rowLoop(rowFlips + count, rowIndex + 1)
      }
    }
  }

  let collectColumnValues = (columnIndex: int) => {
    let rec collect = (columnValues: array<int>, rowIndex: int) => {
      switch rowIndex === maxRows {
      | true => columnValues
      | false => {
          let value =
            grid
            ->Array.at(rowIndex)
            ->Option.flatMap(row => row->Array.at(columnIndex))
            ->Option.getOr(-1)

          collect(columnValues->Array.concat([value]), rowIndex + 1)
        }
      }
    }

    collect([], 0)
  }

  let rec columnLoop = (columnFlips: int, columnIndex: int) => {
    switch columnIndex === maxColumns {
    | true => columnFlips
    | false => {
        let columnValues = collectColumnValues(columnIndex)
        let count = countPalindromicFlips(columnValues)

        columnLoop(columnFlips + count, columnIndex + 1)
      }
    }
  }

  let rowFlips = rowLoop(0, 0)
  let columnFlips = columnLoop(0, 0)

  rowFlips < columnFlips ? rowFlips : columnFlips
}

let g1 = [[1, 0, 0], [0, 0, 0], [0, 0, 1]]
let r1 = minNumberOfFlipsRequiredToMakeBinaryGridPalindromic_I(g1)
Console.log2("r1: ", r1) // 2

let g2 = [[0, 1], [0, 1], [0, 0]]
let r2 = minNumberOfFlipsRequiredToMakeBinaryGridPalindromic_I(g2)
Console.log2("r2: ", r2) // 1

let g3 = [[1], [0]]
let r3 = minNumberOfFlipsRequiredToMakeBinaryGridPalindromic_I(g3)
Console.log2("r3: ", r3) // 0
