// T(n) = O(n)
// S(n) = O(n)

let finalPricesSpecialDiscount = (prices: array<int>) => {
  let length = Array.length(prices)

  let rec updateMonoIncrStack = (
    discounts: array<int>,
    indexToPush: int,
    monoIncrStack: array<int>,
  ) => {
    let stackLength = Array.length(monoIncrStack)

    let numToPush = switch prices->Array.get(indexToPush) {
    | None => Int32.min_int
    | Some(num) => num
    }

    let prevIndex = switch monoIncrStack->Array.at(-1) {
    | None => Int32.min_int
    | Some(num) => num
    }
    let prevMaximum = switch prices->Array.get(prevIndex) {
    | None => Int32.min_int + 1
    | Some(num) => num
    }

    switch numToPush > prevMaximum || stackLength < 1 {
    | true => (discounts, monoIncrStack->Array.concat([indexToPush]))
    | false => {
        discounts->Array.set(prevIndex, prevMaximum - numToPush)

        updateMonoIncrStack(
          discounts,
          indexToPush,
          monoIncrStack->Array.slice(~start=0, ~end=stackLength - 1),
        )
      }
    }
  }

  let rec loop = (discounts: array<int>, index: int, monoIncrStack: array<int>) => {
    switch index === length {
    | true => (discounts, monoIncrStack)
    | false => {
        let (updatedDiscounts, updatedMonoIncrStack) = updateMonoIncrStack(
          discounts,
          index,
          monoIncrStack,
        )

        loop(updatedDiscounts, index + 1, updatedMonoIncrStack)
      }
    }
  }

  let (discounts, monoIncrStack) = loop(Array.make(~length, -1), 0, [])

  switch Array.length(monoIncrStack) > 0 {
  // prices of remaining indexes do not have discounts
  | true =>
    monoIncrStack->Array.reduceRight(discounts, (acc, currIndex) => {
      let price = switch prices->Array.get(currIndex) {
      | None => Int32.min_int
      | Some(num) => num
      }

      acc->Array.set(currIndex, price)
      acc
    })
  | false => discounts
  }
}

let p1 = [8, 4, 6, 2, 3]
let r1 = finalPricesSpecialDiscount(p1)
Console.log2("r1", r1)

let p2 = [1, 2, 3, 4, 5]
let r2 = finalPricesSpecialDiscount(p2)
Console.log2("r2", r2)

let p3 = [10, 1, 1, 6]
let r3 = finalPricesSpecialDiscount(p3)
Console.log2("r3", r3)
