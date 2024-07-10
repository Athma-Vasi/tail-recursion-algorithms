let pascalsTriangle = (numRows: int): array<array<int>> => {
  let rec accLoop = (accumulator: array<array<int>>, height: int) => {
    let prevRow = switch accumulator->Array.get(height - 1) {
    | None => []
    | Some(row) => row
    }
    let length = Array.length(prevRow)

    let rec rowLoop = (currentRow: array<int>, index: int): array<int> => {
      let leftNum = switch prevRow->Array.get(index) {
      | None => Int32.min_int
      | Some(num) => num
      }
      let rightNum = switch prevRow->Array.get(index + 1) {
      | None => Int32.min_int
      | Some(num) => num
      }
      let sum = leftNum + rightNum
      let rowClone = currentRow->Array.mapWithIndex((num, idx) => idx === index + 1 ? sum : num)

      index === length - 1 ? currentRow : rowLoop(rowClone, index + 1)
    }

    let currentRow = Array.make(~length=height + 1, 1)
    let newRow = rowLoop(currentRow, 0)
    let accClone = accumulator->Array.map(num => num)
    accClone->Array.push(newRow)

    height === numRows - 1 ? accClone : accLoop(accClone, height + 1)
  }

  let initialTriangle = [[1], [1, 1]]
  accLoop(initialTriangle, 2)
}

let numRows1 = 5
let result1 = pascalsTriangle(5)
Console.log2("5", result1)
