// T(n) = O(1)
// S(n) = O(1)

let accountBalanceAfterRoundedPurchase = (purchaseAmount: int) => {
  let last = Float.mod(Int.toFloat(purchaseAmount), 10.0)
  let newAmount =
    last < 5.0 ? purchaseAmount - Int.fromFloat(last) : purchaseAmount + (10 - Int.fromFloat(last))

  100 - newAmount
}

let p1 = 9
let r1 = accountBalanceAfterRoundedPurchase(p1)
Console.log2("r1: ", r1) // 90

let p2 = 15
let r2 = accountBalanceAfterRoundedPurchase(p2)
Console.log2("r2: ", r2) // 80

let p3 = 10
let r3 = accountBalanceAfterRoundedPurchase(p3)
Console.log2("r3: ", r3) // 90
