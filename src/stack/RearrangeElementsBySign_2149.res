// T(n) = O(n)
// S(n) = O(n)

let rearrangeElementsBySign = (nums: array<int>) => {
  let mergeStacks = (negativeStack, positiveStack) => {
    let rec merge = (merged: array<int>, index: int) => {
      switch index === Array.length(negativeStack) {
      | true => merged
      | false => {
          let negative = switch negativeStack->Array.at(index) {
          | None => Int32.max_int
          | Some(n) => n
          }
          let positive = switch positiveStack->Array.at(index) {
          | None => Int32.min_int
          | Some(n) => n
          }

          merge(merged->Array.concat([positive, negative]), index + 1)
        }
      }
    }

    merge([], 0)
  }

  let rec makeStacks = (negativeStack: array<int>, positiveStack: array<int>, index: int) => {
    switch index === Array.length(nums) {
    | true => mergeStacks(negativeStack, positiveStack)
    | false => {
        let num = switch nums->Array.at(index) {
        | None => 0
        | Some(n) => n
        }

        num < 0
          ? makeStacks(negativeStack->Array.concat([num]), positiveStack, index + 1)
          : makeStacks(negativeStack, positiveStack->Array.concat([num]), index + 1)
      }
    }
  }

  makeStacks([], [], 0)
}

let n1 = [3, 1, -2, -5, 2, -4]
let r1 = rearrangeElementsBySign(n1)
Console.log2("r1: ", r1) // [3,-2,1,-5,2,-4]

let n2 = [-1, 1]
let r2 = rearrangeElementsBySign(n2)
Console.log2("r2: ", r2) // [1,-1]
