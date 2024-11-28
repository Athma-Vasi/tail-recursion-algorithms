// INCOMPLETE

let longestUnequalAdjacentGroupsSubsequence_I = (words: array<string>, groups: array<int>) => {
  let rec findAlternatingSubsequences = (
    indicesAnswer: array<array<int>>,
    temp: array<int>,
    index: int,
    indicesStack: array<int>,
  ) => {
    switch index === Array.length(groups) {
    | true => indicesAnswer
    | false => {
        let curr = switch groups->Array.at(index) {
        | None => 2 // groups is a binary array
        | Some(n) => n
        }
        let prevIdx = switch indicesStack->Array.at(-1) {
        | None => -1
        | Some(n) => n
        }

        switch prevIdx < 0 {
        | true =>
          findAlternatingSubsequences(
            indicesAnswer,
            temp,
            index + 1,
            indicesStack->Array.concat([index]),
          )
        | false => {
            let prev = switch groups->Array.at(prevIdx) {
            | None => 2
            | Some(n) => n
            }

            // r1:  [
            //   [ 1 ]
            // ]
            // r2:  [
            //   [ 0, 1, 1, 2, 3 ]
            // ]

            switch curr === prev {
            | true => {
                let newAnswer = indicesAnswer->Array.concat([temp->Array.concat([index])])
                findAlternatingSubsequences(
                  newAnswer,
                  [],
                  index + 1,
                  indicesStack->Array.concat([index]),
                )
              }
            | false =>
              findAlternatingSubsequences(
                indicesAnswer,
                temp->Array.concat([prevIdx, index]),
                index + 1,
                indicesStack
                ->Array.slice(~start=0, ~end=Array.length(indicesStack) - 1)
                ->Array.concat([index]),
              )
            }
          }
        }
      }
    }
  }

  findAlternatingSubsequences([], [], 0, [])
}

let w1 = ["e", "a", "b"]
let g1 = [0, 0, 1]
let r1 = longestUnequalAdjacentGroupsSubsequence_I(w1, g1)
Console.log2("r1: ", r1) // ["e", "b"]

let w2 = ["a", "b", "c", "d"]
let g2 = [1, 0, 1, 1]
let r2 = longestUnequalAdjacentGroupsSubsequence_I(w2, g2)
Console.log2("r2: ", r2) // ["a", "b", "c"]
