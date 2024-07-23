// T(n) = O(n)
// S(n) = O(n)

let oneThreeTwoPattern = (nums: array<int>) => {
  let length = Array.length(nums)

  let rec updateSetAndStack = (
    existsSet: Set.t<int>,
    monoIncrStack: array<int>,
    numToPush: int,
  ) => {
    let stackLength = Array.length(monoIncrStack)

    let prevMaximum = switch monoIncrStack->Array.at(-1) {
    | None => Int32.min_int
    | Some(num) => num
    }

    switch stackLength < 1 || numToPush > prevMaximum {
    | true => (existsSet, monoIncrStack->Array.concat([numToPush]))
    | false =>
      switch stackLength > 1 {
      // min length of 2 : pattern exists
      | true => {
          existsSet->Set.add(1)

          updateSetAndStack(
            existsSet,
            monoIncrStack->Array.slice(~start=0, ~end=stackLength - 1),
            numToPush,
          )
        }
      // stack length == 1 : pattern cannot exist
      | false =>
        updateSetAndStack(
          existsSet,
          monoIncrStack->Array.slice(~start=0, ~end=stackLength - 1),
          numToPush,
        )
      }
    }
  }

  let rec loop = (existsSet: Set.t<int>, monoIncrStack: array<int>, index: int) => {
    switch index === length {
    | true => Set.size(existsSet) > 0
    | false => {
        let numToPush = switch nums->Array.get(index) {
        | None => Int32.min_int
        | Some(num) => num
        }

        let (updatedExistsSet, updatedMonoIncrStack) = updateSetAndStack(
          existsSet,
          monoIncrStack,
          numToPush,
        )

        loop(updatedExistsSet, updatedMonoIncrStack, index + 1)
      }
    }
  }

  loop(Set.make(), [], 0)
}

let n1 = [1, 2, 3, 4]
let r1 = oneThreeTwoPattern(n1)
Console.log2("r1: ", r1)

let n2 = [3, 1, 4, 2]
let r2 = oneThreeTwoPattern(n2)
Console.log2("r2: ", r2)

let n3 = [-1, 3, 2, 0]
let r3 = oneThreeTwoPattern(n3)
Console.log2("r3: ", r3)
