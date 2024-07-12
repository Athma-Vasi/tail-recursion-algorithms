let moveZeroes = (nums: array<int>) => {
  let length = Array.length(nums)

  let rec loop = (accumulator: array<int>, leftIndex: int, rightIndex: int) => {
    // 0,1,0,3,12
    // ^ ^
    // 1,0,0,3,12
    // ^ ^
    // 1,0,0,3,12
    //   ^ ^
    // 1,0,0,3,12
    //   ^   ^
    // 1,3,0,0,12
    //   ^   ^
    // 1,3,0,0,12
    //     ^ ^
    // 1,3,0,0,12
    //     ^   ^
    // 1,3,12,0,0
    //      ^   ^
    // 1,3,12,0,0
    //        ^ ^

    let leftNum = switch nums->Array.get(leftIndex) {
    | None => Int32.min_int
    | Some(num) => num
    }
    let rightNum = switch nums->Array.get(rightIndex) {
    | None => Int32.min_int
    | Some(num) => num
    }

    switch rightIndex === length {
    | true => nums
    | false =>
      switch leftNum === 0 {
      | true =>
        switch rightNum === 0 {
        | true => loop(accumulator, leftIndex, rightIndex + 1)
        | false => {
            // swap
            nums->Array.set(leftIndex, rightNum)
            nums->Array.set(rightIndex, leftNum)

            // determine distance to move
            let diff = rightIndex - leftIndex
            switch diff === 1 {
            | true => loop(accumulator, leftIndex + 1, rightIndex + 1) // move both by 1
            | false => loop(accumulator, leftIndex + 1, rightIndex) // move left by 1
            }
          }
        }
      | false => {
          // there has been a swap, determine distance to move
          let diff = rightIndex - leftIndex
          switch diff === 1 {
          | true => loop(accumulator, leftIndex + 1, rightIndex + 1) // move both by 1
          | false => loop(accumulator, leftIndex + 1, rightIndex) // move left by 1
          }
        }
      }
    }
  }

  loop(nums, 0, 1)
}

let n1 = [0, 1, 0, 3, 12]
let r1 = moveZeroes(n1)
Console.log2("r1", r1)
