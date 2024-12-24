// INCORRECT - infinite loop

type change = Peak | Trough | Flat

let longestAlternatingSubarray = (nums: array<int>) => {
  let rec slideWindow = (
    subarrays: array<array<int>>,
    stack: array<int>,
    change: change,
    leftIndex: int,
    rightIndex: int,
  ) => {
    switch rightIndex >= Array.length(nums) {
    | true => subarrays
    | false =>
      switch leftIndex === rightIndex {
      | true => slideWindow(subarrays, stack, change, leftIndex, rightIndex + 1)
      | false => {
          // let leftNum = nums->Array.at(leftIndex)->Option.mapOr(0, n => n)
          let rightNum = nums->Array.at(rightIndex)->Option.mapOr(0, n => n)
          let prevNum = stack->Array.at(-1)->Option.mapOr(0, n => n)

          switch change {
          | Peak =>
            switch rightNum - prevNum === -1 {
            | true =>
              slideWindow(
                subarrays,
                stack->Array.concat([rightNum]),
                Trough,
                leftIndex,
                rightIndex + 1,
              )
            | false =>
              slideWindow(
                subarrays->Array.concat([stack]),
                [prevNum],
                Peak,
                rightIndex - 1,
                rightIndex,
              )
            }
          | Trough =>
            switch rightNum - prevNum === 1 {
            | true =>
              slideWindow(
                subarrays,
                stack->Array.concat([rightNum]),
                Peak,
                leftIndex,
                rightIndex + 1,
              )
            | false =>
              slideWindow(
                subarrays->Array.concat([stack]),
                [prevNum],
                Trough,
                rightIndex - 1,
                rightIndex,
              )
            }
          | Flat => {
              let first = nums->Array.at(rightIndex)->Option.mapOr(0, n => n)
              let second = nums->Array.at(rightIndex + 1)->Option.mapOr(0, n => n)

              switch second - first === 1 {
              | true =>
                slideWindow(subarrays, [first, second], Peak, rightIndex + 1, rightIndex + 2)
              | false =>
                switch second - first === -1 {
                | true =>
                  slideWindow(subarrays, [first, second], Trough, rightIndex + 1, rightIndex + 2)
                | false => slideWindow(subarrays, [], Flat, rightIndex + 2, rightIndex + 3)
                }
              }
            }
          }
        }
      }
    }
  }

  let first = nums->Array.at(0)->Option.mapOr(0, n => n)
  let second = nums->Array.at(1)->Option.mapOr(0, n => n)

  slideWindow(
    [],
    second - first === -1 || second - first === 1 ? [first, second] : [second],
    second - first === -1 ? Trough : second - first === 1 ? Peak : Flat,
    second - first === -1 || second - first === 1 ? 0 : 1,
    2,
  )
}

let n1 = [2, 3, 4, 3, 4]
let r1 = longestAlternatingSubarray(n1)
Console.log2("r1: ", r1)
