// T(n) = O(n)
// S(n) = O(n)

let removeKDigits = (digits: string, k: int) => {
  let length = String.length(digits)

  let rec updateMonoIncrStack = (monoIncrStack: array<int>, currentNum: int, counter: int) => {
    let stackLength = Array.length(monoIncrStack)

    let prevMaximum = switch monoIncrStack->Array.at(-1) {
    | None => Int32.min_int + 1
    | Some(num) => num
    }

    switch stackLength < 1 || currentNum >= prevMaximum || counter === k {
    | true => (monoIncrStack->Array.concat([currentNum]), counter)
    | false =>
      updateMonoIncrStack(
        monoIncrStack->Array.slice(~start=0, ~end=stackLength - 1),
        currentNum,
        counter + 1,
      )
    }
  }

  let rec loop = (monoIncrStack: array<int>, counter: int, index: int) => {
    let currentNum = switch digits->String.get(index) {
    | None => Int32.min_int
    | Some(digit) =>
      switch Int.fromString(digit) {
      | None => Int32.min_int
      | Some(num) => num
      }
    }

    switch index === length {
    | true => monoIncrStack
    | false =>
      switch counter > k {
      | true => loop(monoIncrStack->Array.concat([currentNum]), counter, index + 1)
      | false => {
          let (updatedMonoIncrStack, newCounter) = updateMonoIncrStack(
            monoIncrStack,
            currentNum,
            counter,
          )

          loop(updatedMonoIncrStack, newCounter, index + 1)
        }
      }
    }
  }

  let str = loop([], 0, 0)->Array.map(num => Int.toString(num))->Array.join("")

  let rec trimLeadingZeroes = (stop: bool, str: string) => {
    let strLength = String.length(str)

    switch stop {
    | true => str
    | false => {
        let firstChar = switch str->String.get(0) {
        | None => ""
        | Some(char) => char
        }

        switch firstChar === "0" {
        | true => trimLeadingZeroes(false, str->String.slice(~start=1, ~end=strLength))
        | false => trimLeadingZeroes(true, str)
        }
      }
    }
  }

  let trimmedStr = trimLeadingZeroes(false, str)

  switch String.length(trimmedStr) < 1 {
  | true => "0"
  | false => trimmedStr
  }
}

let d1 = "1432219"
let k1 = 3
let r1 = removeKDigits(d1, k1)
Console.log2("r1", r1)

let d2 = "10200"
let k2 = 1
let r2 = removeKDigits(d2, k2)
Console.log2("r2", r2)

let d3 = "10"
let k3 = 2
let r3 = removeKDigits(d3, k3)
Console.log2("r3", r3)
