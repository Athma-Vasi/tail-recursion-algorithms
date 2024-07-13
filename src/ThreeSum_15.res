let threeSum = (nums: array<int>) => {
  let length = Array.length(nums)
  let clone = nums->Array.map(num => num)
  clone->Array.sort((a, b) => float(a - b))

  let rec anchorLoop = (triplets: Set.t<(int, int, int)>, anchorIndex: int) => {
    let prevAnchor = switch clone->Array.get(anchorIndex - 1) {
    | None => Int32.min_int
    | Some(num) => num
    }
    let currentAnchor = switch clone->Array.get(anchorIndex) {
    | None => Int32.min_int
    | Some(num) => num
    }

    let rec explorersLoop = (lowIndex: int, highIndex: int): unit => {
      let lowExplorer = switch clone->Array.get(lowIndex) {
      | None => Int32.min_int
      | Some(num) => num
      }

      let highExplorer = switch clone->Array.get(highIndex) {
      | None => Int32.min_int
      | Some(num) => num
      }

      let sum = currentAnchor + lowExplorer + highExplorer

      switch lowIndex === highIndex ||
      lowIndex === length ||
      highIndex === 0 ||
      lowIndex > highIndex {
      | true => ()
      | false =>
        switch sum === 0 {
        | true => {
            triplets->Set.add((currentAnchor, lowExplorer, highExplorer))
            explorersLoop(lowIndex + 1, highIndex - 1)
          }
        | false =>
          switch sum < 0 {
          | true => explorersLoop(lowIndex + 1, highIndex)
          | false => explorersLoop(lowIndex, highIndex - 1)
          }
        }
      }
    }

    switch prevAnchor === currentAnchor {
    | true => ()
    | false => explorersLoop(anchorIndex + 1, length - 1)
    }

    switch anchorIndex === length - 2 {
    | true => triplets->Set.values->Core__Iterator.toArray
    | false => anchorLoop(triplets, anchorIndex + 1)
    }
  }

  anchorLoop(Set.make(), 0)
}

let n1 = [-1, 0, 1, 2, -1, -4]
let r1 = threeSum(n1)
Console.log2("[-1,0,1,2,-1,-4]", r1)
