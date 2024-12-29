// T(n) = O(n)
// S(n) = O(n)

let findTheDistinctDifferenceArray = (nums: array<int>) => {
  let rec makeDiffArray = (diffArray: array<int>, index: int) => {
    switch index === Array.length(nums) {
    | true => diffArray
    | false => {
        let prefixes = nums->Array.slice(~start=0, ~end=index + 1)->Set.fromArray->Set.size
        let suffixes = nums->Array.sliceToEnd(~start=index + 1)->Set.fromArray->Set.size

        makeDiffArray(diffArray->Array.concat([prefixes - suffixes]), index + 1)
      }
    }
  }

  makeDiffArray([], 0)
}

let n1 = [1, 2, 3, 4, 5]
let r1 = findTheDistinctDifferenceArray(n1)
Console.log2("r1: ", r1) // [-3,-1,1,3,5]

let n2 = [3, 2, 3, 4, 2]
let r2 = findTheDistinctDifferenceArray(n2)
Console.log2("r2: ", r2) // [0,2,0,2,0]
