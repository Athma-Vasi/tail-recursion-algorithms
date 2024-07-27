let findMiddleIndex = (nums: array<int>) => {
  let length = Array.length(nums)
  let total = nums->Array.reduce(0, (acc, num) => acc + num)

  let rec loop = (index: int, leftSum: int) => {
    let currentNum = switch nums->Array.at(index) {
    | None => Int32.max_int
    | Some(num) => num
    }
    let newLeftSum = leftSum + currentNum
    let rightSum = total - newLeftSum

    switch leftSum === rightSum {
    | true => index
    | false => index === length - 1 ? -1 : loop(index + 1, newLeftSum)
    }
  }

  loop(0, 0)
}

let n1 = [2, 3, -1, 8, 4]
let r1 = findMiddleIndex(n1)
Console.log2("[2,3,-1,8,4]", r1)

let n2 = [1, -1, 4]
let r2 = findMiddleIndex(n2)
Console.log2("[1,-1,4]", r2)

let n3 = [2, 5]
let r3 = findMiddleIndex(n3)
Console.log2("[2,5]", r3)
