let findGreatestCommonDivisorOfArray = (nums: array<int>) => {
  let rec findGCD = (n1: int, n2: int) =>
    n2 === 0 ? n1 : findGCD(n2, Float.mod(Int.toFloat(n1), Int.toFloat(n2))->Float.toInt)

  let min = nums->Array.reduce(Int32.max_int, (acc, num) => num < acc ? num : acc)
  let max = nums->Array.reduce(Int32.min_int, (acc, num) => num > acc ? num : acc)

  findGCD(min, max)
}

let n1 = [2, 5, 6, 9, 10]
let r1 = findGreatestCommonDivisorOfArray(n1)
Console.log2("r1: ", r1)

let n2 = [7, 5, 6, 8, 3]
let r2 = findGreatestCommonDivisorOfArray(n2)
Console.log2("r2: ", r2)

let n3 = [3, 3]
let r3 = findGreatestCommonDivisorOfArray(n3)
Console.log2("r3: ", r3)
