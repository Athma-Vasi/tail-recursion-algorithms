// T(n) = O(n)
// S(n) = O(n)

let maximumDistanceInArrays = (nums: array<array<int>>) => {
  let rec makeMonoIncrStack = (monoIncrStack: array<int>, tempStack: array<int>, index: int) => {
    switch index === Array.length(nums) {
    | true => monoIncrStack->Array.concat(tempStack)
    | false => {
        let arr = switch nums->Array.at(index) {
        | None => []
        | Some(a) => a
        }

        let rec innerLoop = (monoStack, tStack, innerIdx: int) => {
          let num = switch arr->Array.at(innerIdx) {
          | None => Int32.min_int + 1
          | Some(n) => n
          }

          let prevNum = switch monoStack->Array.at(-1) {
          | None => Int32.min_int
          | Some(n) => n
          }

          switch innerIdx === Array.length(arr) {
          | true => (monoStack, tStack)
          | false =>
            switch num === Int32.min_int + 1 {
            | true => ([], [])
            | false =>
              switch prevNum < num {
              // if previous number is less than current number, put it in tempstack
              | true => {
                  let tStackLength = Array.length(tStack)

                  switch tStackLength === 0 {
                  // if tempstack is empty, push current number to monostack
                  | true => innerLoop(monoStack->Array.concat([num]), tStack, innerIdx + 1)
                  // else compare with top element in tempstack to determine which to push
                  | false => {
                      let topTempNum = switch tStack->Array.at(0) {
                      | None => Int32.min_int
                      | Some(n) => n
                      }

                      switch topTempNum > num {
                      // if top element in tempstack is greater than current number, push it in tempstack
                      | true => innerLoop(monoStack->Array.concat([num]), tStack, innerIdx + 1)
                      // else push top element in tempstack to monostack and push current number to tempstack
                      | false =>
                        innerLoop(
                          monoStack->Array.concat([topTempNum]),
                          tStack->Array.slice(~start=1, ~end=tStackLength),
                          innerIdx + 1,
                        )
                      }
                    }
                  }
                }
              // if previous number is greater than or equal to current number, remove largest element from monostack
              // and unshift it to tempstack
              | false =>
                innerLoop(
                  monoStack->Array.slice(~start=0, ~end=Array.length(monoStack) - 1),
                  [prevNum]->Array.concat(tStack),
                  innerIdx,
                )
              }
            }
          }
        }

        let (newMonoIncrStack, newTempStack) = innerLoop(monoIncrStack, tempStack, 0)
        makeMonoIncrStack(newMonoIncrStack, newTempStack, index + 1)
      }
    }
  }

  let monoIncrStack = makeMonoIncrStack([], [], 0)
  let smallest = switch monoIncrStack->Array.at(0) {
  | None => Int32.min_int
  | Some(n) => n
  }
  let largest = switch monoIncrStack->Array.at(-1) {
  | None => Int32.min_int
  | Some(n) => n
  }

  largest - smallest
}

let a1 = [[1, 2, 3], [4, 5], [1, 2, 3]]
let r1 = maximumDistanceInArrays(a1)
Console.log2("r1: ", r1) // 4

let a2 = [[1, 2, 3], [7, 8], [4, 5, 6]]
let r2 = maximumDistanceInArrays(a2)
Console.log2("r2: ", r2) // 7

let a3 = [[1], [1]]
let r3 = maximumDistanceInArrays(a3)
Console.log2("r3: ", r3) // 0
