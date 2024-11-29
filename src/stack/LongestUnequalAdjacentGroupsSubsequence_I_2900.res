// T(n) = O(n)
// S(n) = O(n)

let longestUnequalAdjacentGroupsSubsequence_I = (words: array<string>, groups: array<int>) => {
  let rec findAlternatingSubsequences = (
    indicesAnswer: array<array<int>>,
    indicesSet: Set.t<int>,
    stack: array<int>,
    currIndex: int,
  ) => {
    switch currIndex >= Array.length(groups) {
    | true =>
      Set.size(indicesSet) > 0
        ? indicesAnswer->Array.concat([indicesSet->Set.values->Core__Iterator.toArray])
        : indicesAnswer
    | false =>
      switch currIndex === 0 {
      | true =>
        findAlternatingSubsequences(
          indicesAnswer,
          indicesSet,
          stack->Array.concat([
            switch groups->Array.at(currIndex) {
            | None => 2 // groups is a binary array
            | Some(n) => n
            },
          ]),
          currIndex + 1,
        )
      | false => {
          let curr = switch groups->Array.at(currIndex) {
          | None => 2 // groups is a binary array
          | Some(n) => n
          }

          let prev = switch stack->Array.at(-1) {
          | None => -1
          | Some(n) => n
          }

          switch prev < 0 {
          | true =>
            findAlternatingSubsequences(
              indicesAnswer,
              indicesSet,
              stack->Array.concat([curr]),
              currIndex + 1,
            )
          | false =>
            switch curr === prev {
            | true =>
              switch Set.size(indicesSet) > 0 {
              | true => {
                  let flushed = indicesSet->Set.values->Core__Iterator.toArray

                  findAlternatingSubsequences(
                    indicesAnswer->Array.concat([flushed]),
                    Set.make(),
                    stack->Array.concat([curr]),
                    currIndex + 1,
                  )
                }
              | false =>
                findAlternatingSubsequences(
                  indicesAnswer,
                  indicesSet,
                  stack->Array.concat([curr]),
                  currIndex + 1,
                )
              }
            | false => {
                indicesSet->Set.add(currIndex - 1)
                indicesSet->Set.add(currIndex)

                findAlternatingSubsequences(
                  indicesAnswer,
                  indicesSet,
                  stack->Array.concat([curr]),
                  currIndex + 1,
                )
              }
            }
          }
        }
      }
    }
  }

  let subsequenceIndicesArray = findAlternatingSubsequences([], Set.make(), [], 0)
  let subsequenceIndices = switch subsequenceIndicesArray->Array.at(0) {
  | None => []
  | Some(arr) => arr
  }
  subsequenceIndices->Array.reduce([], (result, subIdx) => {
    let word = switch words->Array.at(subIdx) {
    | None => String.make()
    | Some(w) => w
    }
    result->Array.concat([word])
  })
}

let w1 = ["e", "a", "b"]
let g1 = [0, 0, 1]
let r1 = longestUnequalAdjacentGroupsSubsequence_I(w1, g1)
Console.log2("r1: ", r1) // ["e", "b"]

let w2 = ["a", "b", "c", "d"]
let g2 = [1, 0, 1, 1]
let r2 = longestUnequalAdjacentGroupsSubsequence_I(w2, g2)
Console.log2("r2: ", r2) // ["a", "b", "c"]
