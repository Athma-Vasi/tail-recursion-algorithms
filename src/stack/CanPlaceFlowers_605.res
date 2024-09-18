// T(n) = O(n)
// S(n) = O(n)

let canPlaceFlowers = (flowerbed: array<int>, n: int) => {
  let rec loop = (flowersRemaining: int, stack: array<int>, index: int) => {
    switch index === Array.length(flowerbed) {
    | true => flowersRemaining
    | false => {
        let prevItemInPlot = switch stack->Array.at(-1) {
        | None => -1
        | Some(i) => i
        }

        let currItemInPlot = switch flowerbed->Array.at(index) {
        | None => -1
        | Some(i) => i
        }

        let nextItemInPlot = switch flowerbed->Array.at(index + 1) {
        | None => -1
        | Some(i) => i
        }

        switch prevItemInPlot === 0 && currItemInPlot === 0 && nextItemInPlot === 0 {
        | true => loop(flowersRemaining - 1, stack->Array.concat([1]), index + 1)
        | false => loop(flowersRemaining, stack->Array.concat([currItemInPlot]), index + 1)
        }
      }
    }
  }

  loop(n, [], 0) === 0
}

let f1 = [1, 0, 0, 0, 1]
let n1 = 1
let r1 = canPlaceFlowers(f1, n1)
Console.log2("r1=", r1)

let f2 = [1, 0, 0, 0, 1]
let n2 = 2
let r2 = canPlaceFlowers(f2, n2)
Console.log2("r2=", r2)
