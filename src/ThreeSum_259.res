let threeSum259 = (nums: array<int>, target: int): int => {
  let length = Array.length(nums)
  let clone = nums->Array.map(num => num)
  clone->Array.sort((a, b) => float(a - b))

  let rec anchorLoop = (result: int, anchorIndex: int) => {
    let currentAnchor = switch clone->Array.get(anchorIndex) {
    | None => Int32.min_int
    | Some(num) => num
    }

    Console.log("--anchorloop")
    Console.log2("currentAnchor", currentAnchor)
    Console.log2("anchorIndex", anchorIndex)
    Console.log2("result", result)

    let rec explorersLoop = (amountOfTriplets: int, lowIndex: int, highIndex: int): int => {
      let lowExplorer = switch clone->Array.get(lowIndex) {
      | None => Int32.min_int
      | Some(num) => num
      }

      let highExplorer = switch clone->Array.get(highIndex) {
      | None => Int32.min_int
      | Some(num) => num
      }

      let sum = currentAnchor + lowExplorer + highExplorer

      Console.log("--explorersLoop")
      Console.log2("clone", clone)
      Console.log2("currentAnchor", currentAnchor)
      Console.log2("lowIndex", lowIndex)
      Console.log2("lowExplorer", lowExplorer)
      Console.log2("highIndex", highIndex)
      Console.log2("highExplorer", highExplorer)
      Console.log2("sum", sum)

      switch lowIndex === highIndex ||
      lowIndex === length ||
      highIndex === 0 ||
      lowIndex > highIndex {
      | true => amountOfTriplets
      | false =>
        switch sum < target {
        | true => explorersLoop(amountOfTriplets + 1, lowIndex + 1, highIndex)
        | false =>
          switch sum > target {
          | true => explorersLoop(amountOfTriplets, lowIndex, highIndex - 1)
          | false => explorersLoop(amountOfTriplets, lowIndex + 1, highIndex - 1)
          }
        }
      }
    }

    let amountOfTriplets = explorersLoop(0, anchorIndex + 1, length - 1)

    Console.log2("amountOfTriplets", amountOfTriplets)

    switch anchorIndex === length - 2 {
    | true => result
    | false => anchorLoop(amountOfTriplets, anchorIndex + 1)
    }
  }

  anchorLoop(0, 0)
}

let n1 = [-2, 0, 1, 3]
let r1 = threeSum259(n1, 2)
Console.log2("[-2,0,1,3] 2", r1)
