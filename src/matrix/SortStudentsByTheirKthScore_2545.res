// T(n) = O(n * log(n))
// S(n) = O(n)

let sortStudentsByTheirKthScore = (scores: array<array<int>>, k: int) => {
  scores->Array.toSorted((row1, row2) => {
    let score1 = switch row1->Array.at(k) {
    | None => -1
    | Some(s) => s
    }
    let score2 = switch row2->Array.at(k) {
    | None => -1
    | Some(s) => s
    }

    Int.compare(score2, score1)
  })
}

let s1 = [[10, 6, 9, 1], [7, 5, 11, 2], [4, 8, 3, 15]]
let k1 = 2
let r1 = sortStudentsByTheirKthScore(s1, k1)
Console.log2("r1: ", r1)

let s2 = [[3, 4], [5, 6]]
let k2 = 0
let r2 = sortStudentsByTheirKthScore(s2, k2)
Console.log2("r2: ", r2)
