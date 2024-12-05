// T(n) = O(n * m) where n is the number of rows and m is the length of binary string
// S(n) = O(1)

let numberOfLaserBeamsInABank = (bank: array<string>) => {
  let rec rowLoop = (amount: int, prevDeviceCount: int, rowIndex: int) => {
    switch rowIndex === Array.length(bank) {
    | true => amount
    | false => {
        let row = switch bank->Array.at(rowIndex) {
        | None => String.make()
        | Some(r) => r
        }

        let rec checkDevicesCount = (devicesCount: int, cellIndex: int) => {
          switch cellIndex === String.length(row) {
          | true => devicesCount
          | false => {
              let cell = row->String.charAt(cellIndex)

              switch cell {
              | "0" => checkDevicesCount(devicesCount, cellIndex + 1)
              // "1" as row is binary str
              | _ => checkDevicesCount(devicesCount + 1, cellIndex + 1)
              }
            }
          }
        }

        let devicesCount = checkDevicesCount(0, 0)

        switch devicesCount > 0 {
        | true =>
          rowLoop(
            prevDeviceCount > 0 ? amount + devicesCount * prevDeviceCount : amount,
            devicesCount,
            rowIndex + 1,
          )
        | false => rowLoop(amount, prevDeviceCount, rowIndex + 1)
        }
      }
    }
  }

  rowLoop(0, 0, 0)
}

let b1 = ["011001", "000000", "010100", "001000"]
let r1 = numberOfLaserBeamsInABank(b1)
Console.log2("r1: ", r1)

let b2 = ["000", "111", "000"]
let r2 = numberOfLaserBeamsInABank(b2)
Console.log2("r2: ", r2)
