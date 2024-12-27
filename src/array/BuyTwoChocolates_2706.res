// T(n) = O(n)
// S(n) = O(1)
// translated from editorial solution

let buyTwoChocolates = (prices: array<int>, money: int) => {
  let first = prices->Array.at(0)->Option.mapOr(0, p => p)
  let second = prices->Array.at(1)->Option.mapOr(0, p => p)
  let firstMinimum = first < second ? first : second
  let secondMinimum = first > second ? first : second

  let rec minimize = (firstMinimum: int, secondMinimum: int, index: int) => {
    switch index === Array.length(prices) {
    | true => (firstMinimum, secondMinimum)
    | false => {
        let price = prices->Array.at(index)->Option.mapOr(0, p => p)

        switch price < firstMinimum {
        | true => minimize(price, firstMinimum, index + 1)
        | false =>
          switch price < secondMinimum {
          | true => minimize(price, secondMinimum, index + 1)
          | false => minimize(firstMinimum, secondMinimum, index + 1)
          }
        }
      }
    }
  }

  let (firstMin, secondMin) = minimize(firstMinimum, secondMinimum, 2)
  firstMin + secondMin <= money ? money - (firstMin + secondMin) : money
}

let p1 = [1, 2, 2]
let m1 = 3
let r1 = buyTwoChocolates(p1, m1)
Console.log2("r1: ", r1) // 0

let p2 = [3, 2, 3]
let m2 = 3
let r2 = buyTwoChocolates(p2, m2)
Console.log2("r2: ", r2) // 3
