// T(n) = O(m * n)
// S(n) = O(m + n)

let queriesOnPermutationWithKey = (queries: array<int>, m: int) => {
  let makeRange = (start: int, end: int) => {
    let rec collect = (range: array<int>, limit: int) => {
      switch limit > end {
      | true => range
      | false => collect(range->Array.concat([limit]), limit + 1)
      }
    }

    collect([], start)
  }

  let findIndexOfNum = (nums, num) =>
    nums->Array.reduceWithIndex(-1, (acc, n, idx) => n === num ? idx : acc)

  let removeNum = (nums, index) => nums->Array.filterWithIndex((_num, idx) => idx !== index)

  let range = makeRange(1, m)

  let rec processQueries = (processed: array<int>, shuffledRange: array<int>, index: int) => {
    switch index === Array.length(queries) {
    | true => processed
    | false => {
        let query = switch queries->Array.at(index) {
        | None => -1
        | Some(q) => q
        }

        let queryIdx = findIndexOfNum(shuffledRange, query)
        let removed = removeNum(shuffledRange, queryIdx)

        processQueries(
          processed->Array.concat([queryIdx]),
          [query]->Array.concat(removed),
          index + 1,
        )
      }
    }
  }

  processQueries([], range, 0)
}

let q1 = [3, 1, 2, 1]
let m1 = 5
let r1 = queriesOnPermutationWithKey(q1, m1)
Console.log2("r1: ", r1) // [2, 1, 2, 1]

let q2 = [4, 1, 2, 2]
let m2 = 4
let r2 = queriesOnPermutationWithKey(q2, m2)
Console.log2("r2: ", r2) // [3, 1, 2, 0]

let q3 = [7, 5, 5, 8, 3]
let m3 = 8
let r3 = queriesOnPermutationWithKey(q3, m3)
Console.log2("r3: ", r3) // [6, 5, 0, 7, 5]
