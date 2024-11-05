// T(n) = O(n)
// S(n) = O(1)

let excelSheetColumnTitle = (columnNumber: int) => {
  let rec loop = (columnTitle: string, remainder: float) => {
    switch remainder < 1.0 {
    | true => columnTitle
    | false => {
        let num = Float.mod(remainder -. 1.0, 26.0)
        let str = String.fromCharCode(Float.toInt(num +. 65.0))

        loop(str->String.concat(columnTitle), Math.floor((remainder -. num) /. 26.0))
      }
    }
  }

  loop(String.make(), Int.toFloat(columnNumber))
}

let s1 = 1
let r1 = excelSheetColumnTitle(s1)
Console.log2("r1: ", r1) // "A"

let s2 = 28
let r2 = excelSheetColumnTitle(s2)
Console.log2("r2: ", r2) // "AB"

let s3 = 701
let r3 = excelSheetColumnTitle(s3)
Console.log2("r3: ", r3) // "ZY"
