// T(n) = O(n^2)
// S(n) = O(n)
// incorrect - found closest diff to target, not the sum

let threeSum16 = (nums: array<int>, target: int) => {
  let length = Array.length(nums)
  let clone = nums->Array.map(num => num)
  clone->Array.sort((a, b) => float(a - b))

  let rec anchorLoop = (closest: int, anchorIndex: int): int => {
    let currentAnchor = switch clone->Array.get(anchorIndex) {
    | None => Int32.min_int
    | Some(num) => num
    }

    let rec explorersLoop = (~tempClosest: int, lowIndex: int, highIndex: int) => {
      let lowExplorer = switch clone->Array.get(lowIndex) {
      | None => Int32.min_int
      | Some(num) => num
      }

      let highExplorer = switch clone->Array.get(highIndex) {
      | None => Int32.min_int
      | Some(num) => num
      }

      let sum = currentAnchor + lowExplorer + highExplorer
      let newDelta = Math.abs(Int.toFloat(target - sum))->Float.toInt
      let newClosest = Math.min(Int.toFloat(tempClosest), Int.toFloat(newDelta))->Float.toInt

      switch lowIndex === highIndex ||
      lowIndex === length ||
      highIndex === 0 ||
      lowIndex > highIndex {
      | true => tempClosest
      | false =>
        switch newClosest === target {
        | true => newClosest
        | false =>
          switch newClosest < tempClosest {
          | true => explorersLoop(~tempClosest=newClosest, lowIndex + 1, highIndex)
          | false => explorersLoop(~tempClosest=newClosest, lowIndex, highIndex - 1)
          }
        }
      }
    }

    let newClosest = explorersLoop(~tempClosest=closest, anchorIndex + 1, length - 1)

    switch newClosest === target {
    | true => newClosest
    | false => anchorLoop(newClosest, anchorIndex + 1)
    }
  }

  anchorLoop(Int32.max_int, 0)
}

let n1 = [-1, 2, 1, -4]
let r1 = threeSum16(n1, 1)
Console.log2("[-1,2,1,-4]", r1)
