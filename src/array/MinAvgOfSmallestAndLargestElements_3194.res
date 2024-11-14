// T(n) = O(n)
// S(n) = O(n)

let minAvgOfSmallestAndLargestElements = (nums: array<int>) => {
  let rec loop = (averages: array<float>, shrunkNums: array<int>, count: int) => {
    switch count > Array.length(nums) / 2 {
    | true => averages->Array.reduce(51.0, (min, num) => min < num ? min : num)
    | false => {
        let (minElement, maxElement) = shrunkNums->Array.reduce((51, 0), (tuple, num) => {
          let (min, max) = tuple
          let newMin = min < num ? min : num
          let newMax = max > num ? max : num

          (newMin, newMax)
        })
        let average = (Int.toFloat(minElement) +. Int.toFloat(maxElement)) /. 2.0
        let minIndex = shrunkNums->Array.findIndex(num => num === minElement)
        let maxIndex = shrunkNums->Array.findIndex(num => num === maxElement)

        loop(
          averages->Array.concat([average]),
          shrunkNums->Array.filterWithIndex((_num, index) =>
            index !== minIndex && index !== maxIndex
          ),
          count + 1,
        )
      }
    }
  }

  loop([], Array.copy(nums), 0)
}

let n1 = [7, 8, 3, 4, 15, 13, 4, 1]
let r1 = minAvgOfSmallestAndLargestElements(n1)
Console.log2("r1: ", r1) // 5.5

let n2 = [1, 9, 8, 3, 10, 5]
let r2 = minAvgOfSmallestAndLargestElements(n2)
Console.log2("r2: ", r2) // 5.5

let n3 = [1, 2, 3, 7, 8, 9]
let r3 = minAvgOfSmallestAndLargestElements(n3)
Console.log2("r3: ", r3) // 5.0
