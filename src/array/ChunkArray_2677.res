let chunkArray = (arr: array<int>, size: int) => {
  let rec makeChunks = (chunks: array<array<int>>, chunk: array<int>, index: int) => {
    switch index === Array.length(arr) {
    | true => chunks->Array.concat([chunk])
    | false => {
        let num = arr->Array.at(index)->Option.mapOr(0, n => n)

        switch Array.length(chunk) === size {
        | true => makeChunks(chunks->Array.concat([chunk]), [num], index + 1)
        | false => makeChunks(chunks, chunk->Array.concat([num]), index + 1)
        }
      }
    }
  }

  Array.length(arr) === 0 ? [] : makeChunks([], [], 0)
}

let a1 = [1, 2, 3, 4, 5]
let s1 = 1
let r1 = chunkArray(a1, s1)
Console.log2("r1: ", r1) // [[1], [2], [3], [4], [5]]

let a2 = [1, 9, 6, 3, 2]
let s2 = 3
let r2 = chunkArray(a2, s2)
Console.log2("r2: ", r2) // [[1, 9, 6], [3, 2]]

let a3 = [8, 5, 3, 2, 6]
let s3 = 6
let r3 = chunkArray(a3, s3)
Console.log2("r3: ", r3) // [[8, 5, 3, 2, 6]]

let a4 = []
let s4 = 1
let r4 = chunkArray(a4, s4)
Console.log2("r4: ", r4) // [[]]
