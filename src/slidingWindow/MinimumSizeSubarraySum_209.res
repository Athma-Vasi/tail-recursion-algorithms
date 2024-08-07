// T(n) = O(n)
// S(n) = O(1)

let minimumSizeSubarraySum = (nums: array<int>, target: int) => {
  let rec slideWindow = (~minSize: int, ~sum: int, leftIndex: int, rightIndex: int) => {
    switch rightIndex === Array.length(nums) {
    | true => minSize === Int32.max_int ? 0 : minSize
    | false =>
      switch leftIndex === rightIndex {
      | true => slideWindow(~minSize, ~sum, leftIndex, rightIndex + 1)
      | false => {
          let rightIncludedNum = switch nums->Array.at(rightIndex) {
          | None => Int32.min_int
          | Some(n) => n
          }

          switch rightIncludedNum === target {
          | true => 1
          | false => {
              let newSum = sum + rightIncludedNum

              switch newSum < target {
              | true => slideWindow(~minSize, ~sum=newSum, leftIndex, rightIndex + 1)
              | false => {
                  let leftExcludedNum = switch nums->Array.at(leftIndex) {
                  | None => Int32.min_int
                  | Some(n) => n
                  }
                  let newMinSize = rightIndex - leftIndex + 1

                  slideWindow(
                    ~minSize=newMinSize < minSize ? newMinSize : minSize,
                    ~sum=sum - leftExcludedNum,
                    leftIndex + 1,
                    rightIndex,
                  )
                }
              }
            }
          }
        }
      }
    }
  }

  let initialSum = nums->Array.reduceWithIndex(0, (acc, curr, idx) => idx > 1 ? acc : acc + curr)
  slideWindow(~minSize=Int32.max_int, ~sum=initialSum, 0, 2)
}

let a1 = [2, 3, 1, 2, 4, 3]
let t1 = 7
let r1 = minimumSizeSubarraySum(a1, t1)
Console.log3("r1: ", r1, r1 === 2)

let a2 = [1, 4, 4]
let t2 = 4
let r2 = minimumSizeSubarraySum(a2, t2)
Console.log3("r2: ", r2, r2 === 1)

let a3 = [1, 1, 1, 1, 1, 1, 1, 1]
let t3 = 11
let r3 = minimumSizeSubarraySum(a3, t3)
Console.log3("r3: ", r3, r3 === 0)
