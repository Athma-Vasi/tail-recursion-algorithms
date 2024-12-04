// The distance between two points (x1, y1) and (x2, y2) is (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)
// T(n) = O(n * m) where n is the number of points and m is the number of queries
// S(n) = O(1)
// https://leetcode.com/problems/queries-on-number-of-points-inside-a-circle/solutions/1163432/javascript-es6-math-formula-with-explanation-easy-100-faster/

let queriesOnNumberOfPointsInsideCircle = (
  points: array<(int, int)>,
  queries: array<(int, int, int)>,
) => {
  let rec queriesLoop = (result: array<int>, qIndex: int) => {
    switch qIndex === Array.length(queries) {
    | true => result
    | false => {
        let (x, y, radius) = switch queries->Array.at(qIndex) {
        | None => (0, 0, 0)
        | Some(t) => t
        }

        let rec pointsLoop = (count: int, pIndex: int) => {
          switch pIndex === Array.length(points) {
          | true => count
          | false => {
              let (a, b) = switch points->Array.at(pIndex) {
              | None => (0, 0)
              | Some(t) => t
              }
              let distance = (x - a) * (x - a) + (y - b) * (y - b)
              let isInside = distance <= radius * radius

              pointsLoop(isInside ? count + 1 : count, pIndex + 1)
            }
          }
        }

        let count = pointsLoop(0, 0)
        queriesLoop(result->Array.concat([count]), qIndex + 1)
      }
    }
  }

  queriesLoop([], 0)
}

let p1 = [(1, 3), (3, 3), (5, 3), (2, 2)]
let q1 = [(2, 3, 1), (4, 3, 1), (1, 1, 2)]
let r1 = queriesOnNumberOfPointsInsideCircle(p1, q1)
Console.log2("r1: ", r1) // [3, 2, 2]

let p2 = [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5)]
let q2 = [(1, 2, 2), (2, 2, 2), (4, 3, 2), (4, 3, 3)]
let r2 = queriesOnNumberOfPointsInsideCircle(p2, q2)
Console.log2("r2: ", r2) // [2, 3, 2, 4]
