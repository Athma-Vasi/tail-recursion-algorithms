// T(n) = O(n^2)
// S(n) = O(n)

let minimumNumberGame = (nums: array<int>) => {
  let findMinAndIndex = nums =>
    nums->Array.reduceWithIndex((Int32.max_int, -1), (acc, num, idx) => {
      let (min, index) = acc
      num < min ? (num, idx) : (min, index)
    })

  let removeNumber = (nums, index) => nums->Array.filterWithIndex((_num, idx) => idx !== index)

  let rec play = (result: array<int>, sliced: array<int>) => {
    switch Array.length(sliced) === 0 {
    | true => result
    | false => {
        let (aliceNum, aliceIndex) = findMinAndIndex(sliced)
        let aliceSliced = removeNumber(sliced, aliceIndex)
        let (bobNum, bobIndex) = findMinAndIndex(aliceSliced)
        let bobSliced = removeNumber(aliceSliced, bobIndex)

        play(
          result
          ->Array.concat([bobNum])
          ->Array.concat([aliceNum]),
          bobSliced,
        )
      }
    }
  }

  play([], Array.copy(nums))
}

let n1 = [5, 4, 2, 3]
let r1 = minimumNumberGame(n1)
Console.log2("r1: ", r1) // [3, 2, 5, 4]

let n2 = [2, 5]
let r2 = minimumNumberGame(n2)
Console.log2("r2: ", r2) // [5, 2]
