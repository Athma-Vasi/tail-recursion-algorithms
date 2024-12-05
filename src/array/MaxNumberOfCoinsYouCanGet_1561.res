// T(n) = O(n)
// S(n) = O(n(n+1)/2) = O(n^2)

let maxNumberOfCoinsYouCanGet = (piles: array<int>) => {
  let findLargestAndIndex = nums =>
    nums->Array.reduceWithIndex((Int32.min_int, -1), (acc, num, index) => {
      let (max, idx) = acc
      max > num ? (max, idx) : (num, index)
    })

  let findSmallestAndIndex = nums =>
    nums->Array.reduceWithIndex((Int32.max_int, -1), (acc, num, index) => {
      let (min, idx) = acc
      min < num ? (min, idx) : (num, index)
    })

  let removeNum = (nums, index) => nums->Array.filterWithIndex((_num, idx) => idx !== index)

  let rec choose = (amount: int, sliced: array<int>) => {
    switch Array.length(sliced) < 3 {
    | true => amount
    | false => {
        let (_aliceLargest, aliceIndex) = findLargestAndIndex(sliced)
        let aliceRemoved = removeNum(sliced, aliceIndex)
        let (moiLargest, moiIndex) = findLargestAndIndex(aliceRemoved)
        let moiRemoved = removeNum(aliceRemoved, moiIndex)
        let (_bobLargest, bobIndex) = findSmallestAndIndex(moiRemoved)
        let bobRemoved = removeNum(moiRemoved, bobIndex)

        choose(amount + moiLargest, bobRemoved)
      }
    }
  }

  choose(0, piles->Array.map(p => p))
}

let p1 = [2, 4, 1, 2, 7, 8]
let r1 = maxNumberOfCoinsYouCanGet(p1)
Console.log2("r1: ", r1) // 9

let p2 = [2, 4, 5]
let r2 = maxNumberOfCoinsYouCanGet(p2)
Console.log2("r2: ", r2) // 4

let p3 = [9, 8, 7, 6, 5, 1, 2, 3, 4]
let r3 = maxNumberOfCoinsYouCanGet(p3)
Console.log2("r3: ", r3) // 18
