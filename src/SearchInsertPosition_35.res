// T(n) = O(n)
// S(n) = O(n)

let searchInsertPosition = (nums: array<int>, target: int) => {
  let length = Array.length(nums)

  let rec loop = (foundIndex: int, sliced: array<int>) => {
    let sliceLength = Array.length(sliced)
    let isEvenLength = Float.mod(Int.toFloat(sliceLength), 2.0) == 0.0

    switch foundIndex > -1 {
    | true => foundIndex
    | false =>
      switch isEvenLength {
      | true => {
          let rightIndex = sliceLength / 2
          let rightNum = switch sliced->Array.get(rightIndex) {
          | None => Int32.min_int + 1
          | Some(num) => num
          }

          let leftIndex = rightIndex - 1
          let leftNum = switch sliced->Array.get(leftIndex) {
          | None => Int32.min_int
          | Some(num) => num
          }

          switch rightNum === Int32.min_int + 1 {
          // target greater than rightmost num
          | true => length
          | false =>
            switch leftNum === Int32.min_int {
            // target less than leftmost num
            | true => -1
            | false =>
              switch leftNum === target {
              | true => leftIndex
              | false =>
                switch rightNum === target {
                | true => rightIndex
                | false =>
                  switch leftNum < target && rightNum > target {
                  | true => rightIndex
                  | false =>
                    switch leftNum > target {
                    | true => loop(foundIndex, sliced->Array.slice(~start=0, ~end=rightIndex))
                    | false =>
                      loop(foundIndex, sliced->Array.slice(~start=rightIndex, ~end=sliceLength))
                    }
                  }
                }
              }
            }
          }
        }
      | false => {
          let middleIndex = Math.floor(Int.toFloat(sliceLength) /. 2.0)->Float.toInt
          let middleNum = switch sliced->Array.get(middleIndex) {
          | None => Int32.min_int
          | Some(num) => num
          }

          switch middleNum === target {
          | true => middleIndex
          | false =>
            switch middleNum < target {
            | true =>
              loop(foundIndex, sliced->Array.slice(~start=middleIndex + 1, ~end=sliceLength))
            | false => loop(foundIndex, sliced->Array.slice(~start=0, ~end=middleIndex))
            }
          }
        }
      }
    }
  }

  loop(-1, nums)
}

let n1 = [1, 3, 5, 6]
let t1 = 5
let r1 = searchInsertPosition(n1, t1)
Console.log2("r1: ", r1)

let t2 = 2
let r2 = searchInsertPosition(n1, t2)
Console.log2("r2: ", r2)

let t3 = 7
let r3 = searchInsertPosition(n1, t3)
Console.log2("r3: ", r3)
