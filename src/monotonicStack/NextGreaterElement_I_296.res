// T(n) = O(n + m) where n <= m
// S(n) = O(n)

let nextGreaterElementI = (nums1: array<int>, nums2: array<int>) => {
  let length1 = Array.length(nums1)
  let length2 = Array.length(nums2)

  let rec updateStackAndTable = (
    monoIncrStack: array<int>,
    nextGreaterTable: Map.t<int, int>,
    nums2Index: int,
  ) => {
    let stackLength = Array.length(monoIncrStack)

    let num2 = switch nums2->Array.get(nums2Index) {
    | None => Int32.min_int
    | Some(num2) => num2
    }

    let prevMaximum = switch monoIncrStack->Array.at(-1) {
    | None => Int32.min_int
    | Some(num) => num
    }

    switch nums2Index === length2 {
    // if at the end of nums2, return nextGreaterTable
    | true => nextGreaterTable
    | false =>
      switch stackLength < 1 {
      // if stack is empty, push num2 to stack
      | true => updateStackAndTable([num2], nextGreaterTable, nums2Index + 1)
      | false =>
        // if stack is not empty, compare prevMaximum with num2
        switch prevMaximum > num2 {
        // if prevMaximum > num2, do nothing and continue
        | true => updateStackAndTable(monoIncrStack, nextGreaterTable, nums2Index + 1)
        // if prevMaximum < num2, pop the stack and update nextGreaterTable
        | false => {
            let sliced = monoIncrStack->Array.slice(~start=0, ~end=stackLength - 1)
            let updatedStack = sliced->Array.concat([num2])
            nextGreaterTable->Map.set(prevMaximum, num2) // ~= (num1Element, num2NextGreaterElement)

            updateStackAndTable(updatedStack, nextGreaterTable, nums2Index + 1)
          }
        }
      }
    }
  }

  let rec nums1Loop = (
    nextGreaterElements: array<int>,
    nextGreaterTable: Map.t<int, int>,
    nums1Index: int,
  ) => {
    switch nums1Index === length1 {
    | true => nextGreaterElements
    | false => {
        let num1 = switch nums1->Array.get(nums1Index) {
        | None => Int32.min_int
        | Some(num1) => num1
        }

        let nextGreaterElement = switch nextGreaterTable->Map.get(num1) {
        | None => -1
        | Some(num) => num
        }

        nums1Loop(
          nextGreaterElements->Array.concat([nextGreaterElement]),
          nextGreaterTable,
          nums1Index + 1,
        )
      }
    }
  }

  let nextGreaterTable = updateStackAndTable([], Map.make(), 0)

  nums1Loop([], nextGreaterTable, 0)
}

let n1 = [4, 1, 2]
let n11 = [1, 3, 4, 2]
let r1 = nextGreaterElementI(n1, n11)
Console.log(r1)

let n2 = [2, 4]
let n22 = [1, 2, 3, 4]
let r2 = nextGreaterElementI(n2, n22)
Console.log(r2)
