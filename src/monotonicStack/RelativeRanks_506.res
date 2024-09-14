// T(n) = O(n)
// S(n) = O(n)
// incorrect - infinite loop

let relativeRanks = (scores: array<int>) => {
  let rec updateMonoDecrStack = (
    monoDecrStack: array<(int, int)>,
    tupleToPush: (int, int),
    tempStack: array<(int, int)>,
  ) => {
    let stackLength = Array.length(monoDecrStack)
    let (numToPush, idxOfNum) = tupleToPush

    let (prevMaximum, idxOfPrevMax) = switch monoDecrStack->Array.at(-1) {
    | None => (-1, -1)
    | Some(t) => t
    }

    switch numToPush < prevMaximum {
    | true => (monoDecrStack->Array.concat([(numToPush, idxOfNum)]), tempStack)
    | false =>
      updateMonoDecrStack(
        monoDecrStack
        ->Array.slice(~start=0, ~end=stackLength - 1)
        ->Array.concat([(numToPush, idxOfNum)]),
        tupleToPush,
        tempStack->Array.concat(monoDecrStack->Array.slice(~start=0, ~end=stackLength - 1)),
      )
    }
  }

  let rec scoresLoop = (
    monoDecrStack: array<(int, int)>,
    scoreIndex: int,
    tempStack: array<(int, int)>,
  ) => {
    let score = switch scores->Array.at(scoreIndex) {
    | None => -1
    | Some(s) => s
    }

    let (updatedMonoDecrStack, updatedTempStack) = updateMonoDecrStack(
      monoDecrStack,
      (score, scoreIndex),
      tempStack,
    )

    switch scoreIndex === Array.length(scores) {
    | true => (monoDecrStack, tempStack)
    | false => scoresLoop(updatedMonoDecrStack, scoreIndex + 1, updatedTempStack)
    }
  }

  let (monoDecrStack, tempStack) = scoresLoop([], 0, [])

  let rec flushTempLoop = (
    monoDecrStack: array<(int, int)>,
    index: int,
    tempStack: array<(int, int)>,
  ) => {
    let remainingTuple = switch tempStack->Array.at(index) {
    | None => (-1, -1)
    | Some(t) => t
    }

    switch Array.length(tempStack) === 0 {
    | true => monoDecrStack
    | false => {
        let (updatedMonoDecrStack, updatedTempStack) = updateMonoDecrStack(
          monoDecrStack,
          remainingTuple,
          [],
        )

        flushTempLoop(
          updatedMonoDecrStack,
          index === Array.length(tempStack) ? 0 : index + 1,
          updatedTempStack,
        )
      }
    }
  }

  let sortedDescStack = flushTempLoop(monoDecrStack, 0, tempStack)

  let rec calculateRanks = (
    ranks: array<string>,
    sortedDescStack: array<(int, int)>,
    stackIndex: int,
  ) => {
    let (score, scoreIndex) = switch sortedDescStack->Array.at(stackIndex) {
    | None => (-1, -1)
    | Some(t) => t
    }

    ranks->Array.set(
      scoreIndex,
      scoreIndex === 0
        ? "Gold Medal"
        : scoreIndex === 1
        ? "Silver Medal"
        : scoreIndex === 2
        ? "Bronze Medal"
        : Int.toString(scoreIndex),
    )

    switch stackIndex === Array.length(sortedDescStack) {
    | true => ranks
    | false => calculateRanks(ranks, sortedDescStack, stackIndex + 1)
    }
  }

  calculateRanks(Array.make(~length=Array.length(scores), ""), sortedDescStack, 0)
}

let s1 = [5, 4, 3, 2, 1]
let r1 = relativeRanks(s1)
Console.log2("r1: ", r1) // ["Gold Medal", "Silver Medal", "Bronze Medal", "4", "5"]

let s2 = [10, 3, 8, 9, 4]
let r2 = relativeRanks(s2)
Console.log2("r2: ", r2) // ["Gold Medal", "5", "Bronze Medal", "Silver Medal", "4"]

let s3 = [1, 2, 3, 4, 5]
let r3 = relativeRanks(s3)
Console.log2("r3: ", r3) // ["5", "4", "Bronze Medal", "Silver Medal", "Gold Medal"]
