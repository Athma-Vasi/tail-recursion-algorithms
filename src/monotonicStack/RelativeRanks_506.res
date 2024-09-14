// T(n) = O(n)
// S(n) = O(n)

let relativeRanks = (scores: array<int>) => {
  let updateMonoDecrStack = (
    monoDecrStack: array<(int, int)>,
    tupleToPush: (int, int),
    tempStack: array<(int, int)>,
  ) => {
    let stackLength = Array.length(monoDecrStack)
    let (numToPush, idxOfNum) = tupleToPush

    let (prevMaximum, _idxOfPrevMax) = switch monoDecrStack->Array.at(-1) {
    | None => (Int32.max_int, -1)
    | Some(t) => t
    }

    switch numToPush < prevMaximum || Array.length(monoDecrStack) === 0 {
    | true => (monoDecrStack->Array.concat([(numToPush, idxOfNum)]), tempStack)
    | false => (
        monoDecrStack
        ->Array.slice(~start=0, ~end=stackLength - 1)
        ->Array.concat([(numToPush, idxOfNum)]),
        monoDecrStack
        ->Array.slice(~start=stackLength - 1, ~end=stackLength)
        ->Array.concat(tempStack),
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

    switch scoreIndex === Array.length(scores) - 1 {
    | true => (updatedMonoDecrStack, updatedTempStack)
    | false => scoresLoop(updatedMonoDecrStack, scoreIndex + 1, updatedTempStack)
    }
  }

  let (updatedMonoDecrStack, updatedTempStack) = scoresLoop([], 0, [])

  let rec flushTempLoop = (
    monoDecrStack: array<(int, int)>,
    index: int,
    tempStack: array<(int, int)>,
  ) => {
    switch Array.length(tempStack) === 0 {
    | true => monoDecrStack
    | false => {
        let remainingTuple = switch Array.shift(tempStack) {
        | None => (-1, -1)
        | Some(t) => t
        }

        let (updatedMonoDecrStack, updatedTempStack) = updateMonoDecrStack(
          monoDecrStack,
          remainingTuple,
          tempStack,
        )

        flushTempLoop(
          updatedMonoDecrStack,
          index === Array.length(tempStack) ? 0 : index + 1,
          updatedTempStack,
        )
      }
    }
  }

  let sortedDescStack = flushTempLoop(updatedMonoDecrStack, 0, updatedTempStack)

  let rec calculateRanks = (
    ranks: array<string>,
    sortedDescStack: array<(int, int)>,
    stackIndex: int,
  ) => {
    let (_score, scoreIndex) = switch sortedDescStack->Array.at(stackIndex) {
    | None => (-1, -1)
    | Some(t) => t
    }

    ranks->Array.set(
      scoreIndex,
      stackIndex === 0
        ? "Gold Medal"
        : stackIndex === 1
        ? "Silver Medal"
        : stackIndex === 2
        ? "Bronze Medal"
        : Int.toString(stackIndex + 1),
    )

    switch stackIndex === Array.length(sortedDescStack) - 1 {
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
