// T(n) = O(M(n))
// S(n) = O(1)

let maxAreaOfLongestDiagonalRectangle = (dimensions: array<(int, int)>) => {
  let rec findIdxOfLongestDiagonal = (
    idxOfLongestDiagonal: int,
    longestDiagonal: float,
    index: int,
  ) => {
    switch index === Array.length(dimensions) {
    | true => idxOfLongestDiagonal
    | false => {
        let (length, width) = switch dimensions->Array.at(index) {
        | None => (0, 0)
        | Some(d) => d
        }

        let squareArea = length * length + width * width
        let currDiagonal = Math.sqrt(Int.toFloat(squareArea))
        let (newLongest, newIdx) =
          longestDiagonal > currDiagonal
            ? (longestDiagonal, idxOfLongestDiagonal)
            : (currDiagonal, index)

        findIdxOfLongestDiagonal(newIdx, newLongest, index + 1)
      }
    }
  }

  let idxOfLongestDiagonal = findIdxOfLongestDiagonal(-1, -1.0, 0)
  let (length, width) = switch dimensions->Array.at(idxOfLongestDiagonal) {
  | None => (0, 0)
  | Some(d) => d
  }
  length * width
}

let d1 = [(9, 3), (8, 6)]
let r1 = maxAreaOfLongestDiagonalRectangle(d1)
Console.log2("r1: ", r1) // 48

let d2 = [(3, 4), (4, 3)]
let r2 = maxAreaOfLongestDiagonalRectangle(d2)
Console.log2("r2: ", r2) // 12
