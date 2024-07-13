// T(n) = O(n)
// S(n) = O(1)

let containerMostWater = (heights: array<int>) => {
  let rec loop = (maxArea: int, leftIndex: int, rightIndex: int) => {
    let leftHeight = switch heights->Array.get(leftIndex) {
    | None => Int32.min_int
    | Some(height) => height
    }

    let rightHeight = switch heights->Array.get(rightIndex) {
    | None => Int32.min_int
    | Some(height) => height
    }

    let minHeight = Math.min(Int.toFloat(leftHeight), Int.toFloat(rightHeight))->Float.toInt
    let length = rightIndex - leftIndex
    let area = minHeight * length
    let newMaxArea = Math.max(Int.toFloat(area), Int.toFloat(maxArea))->Float.toInt

    switch leftIndex === rightIndex {
    | true => maxArea
    | false =>
      switch leftHeight < rightHeight {
      | true => loop(newMaxArea, leftIndex + 1, rightIndex)
      | false => loop(newMaxArea, leftIndex, rightIndex - 1)
      }
    }
  }

  loop(Int32.min_int, 0, Array.length(heights) - 1)
}

let h1 = [1, 8, 6, 2, 5, 4, 8, 3, 7]
let r1 = containerMostWater(h1)
Console.log2("[1,8,6,2,5,4,8,3,7]", r1)
