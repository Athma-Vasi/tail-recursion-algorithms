// T(n) = O(n * m)
// S(n) = O(n * m)

let deleteGreatestValueInEachRow = (grid: array<array<int>>) => {
  let rec operation = (answer: int, matrix: array<array<int>>) => {
    let row = matrix->Array.at(0)->Option.mapOr([], r => r)
    let maxColumns = Array.length(row)

    switch maxColumns === 0 {
    | true => answer
    | false => {
        let idxGreatestTuples = matrix->Array.reduce([], (acc, row) => {
          let (idx, greatest) = row->Array.reduceWithIndex((-1, -1), (tuple, val, i) => {
            let (idx, greatest) = tuple
            val > greatest ? (i, val) : (idx, greatest)
          })
          acc->Array.concat([(idx, greatest)])
        })

        let (modified, max) = idxGreatestTuples->Array.reduceWithIndex(([], -1), (
          acc,
          tuple,
          index,
        ) => {
          let (idx, greatest) = tuple
          let row = matrix->Array.at(index)->Option.mapOr([], r => r)
          let reduced = row->Array.filterWithIndex((_val, i) => i !== idx)
          let (modified, max) = acc

          (modified->Array.concat([reduced]), max > greatest ? max : greatest)
        })

        operation(answer + max, modified)
      }
    }
  }

  operation(0, grid)
}

let g1 = [[1, 2, 4], [3, 3, 1]]
let r1 = deleteGreatestValueInEachRow(g1)
Console.log2("r1: ", r1) // 8

let g2 = [[10]]
let r2 = deleteGreatestValueInEachRow(g2)
Console.log2("r2: ", r2) // 10
