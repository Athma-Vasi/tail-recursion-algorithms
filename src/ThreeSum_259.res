// T(n) = O(n^2)
// S(n) = O(1)

let threeSum259 = (nums: array<int>, target: int): int => {
  let length = Array.length(nums)
  let clone = nums->Array.map(num => num)
  clone->Array.sort((a, b) => float(a - b))

  let rec anchorLoop = (result: int, anchorIndex: int) => {
    let currentAnchor = switch clone->Array.get(anchorIndex) {
    | None => Int32.min_int
    | Some(num) => num
    }

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

      switch lowIndex === highIndex {
      | true => amountOfTriplets
      | false =>
        switch sum < target {
        | true => explorersLoop(amountOfTriplets + 1, lowIndex, highIndex - 1)
        | false =>
          switch sum > target {
          | true => explorersLoop(amountOfTriplets, lowIndex + 1, highIndex)
          | false => explorersLoop(amountOfTriplets, lowIndex + 1, highIndex - 1)
          }
        }
      }
    }

    let amountOfTriplets = explorersLoop(result, anchorIndex + 1, length - 1)
    switch anchorIndex === length - 2 {
    | true => amountOfTriplets
    | false => anchorLoop(amountOfTriplets, anchorIndex + 1)
    }
  }

  switch length < 3 {
  | true => 0
  | false => anchorLoop(0, 0)
  }
}

let n1 = [-2, 0, 1, 3]
let r1 = threeSum259(n1, 2)
Console.log2("[-2,0,1,3] 2", r1)
