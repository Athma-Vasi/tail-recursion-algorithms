let moveZeroes = (nums: array<int>) => {
  let clone = nums->Array.map(num => num)

  let rec loop = (leftIndex: int, rightIndex: int) => {
    // 0,1,0,3,12
    // 1,0,0,3,12
    // 1,3,0,0,12
    // if left is zero
    //   if right is zero: increment until non-zero
    //     switch left num with right num
    //     increment left
    // else increment left and right

    let leftNum = switch nums->Array.get(leftIndex) {
    | None => Int32.min_int
    | Some(num) => num
    }
    let rightNum = switch nums->Array.get(rightIndex) {
    | None => Int32.min_int
    | Some(num) => num
    }

    switch leftNum === 0 {
    | true =>
      switch rightNum === 0 {
      | true => loop(leftIndex, rightIndex + 1)
      | false => //   clone->Array.mapWithIndex((num, idx) =>
        // idx === leftIndex ? rightNum : idx === rightIndex ? leftNum : num
        //   )
        loop(leftIndex + 1, rightIndex)
      }
    | false => loop(leftIndex + 1, rightIndex + 1)
    }
  }
}
