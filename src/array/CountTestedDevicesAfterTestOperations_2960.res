// T(n) = O(n)
// S(n) = O(n)

let countTestedDevicesAfterTestOperations = (batteryPercentages: array<int>) => {
  let rec testOperations = (count: int, devices: array<int>) => {
    let length = Array.length(devices)

    switch length === 0 {
    | true => count
    | false => {
        let batteryPercentage = switch devices->Array.at(0) {
        | None => -1
        | Some(bp) => bp
        }
        let sliced = devices->Array.slice(~start=1, ~end=length)

        switch batteryPercentage > 0 {
        | true =>
          testOperations(count + 1, sliced->Array.map(device => device - 1 < 0 ? 0 : device - 1))
        | false => testOperations(count, sliced)
        }
      }
    }
  }

  testOperations(0, batteryPercentages)
}

let bp1 = [1, 1, 2, 1, 3]
let r1 = countTestedDevicesAfterTestOperations(bp1)
Console.log2("r1: ", r1) // 3

let bp2 = [0, 1, 2]
let r2 = countTestedDevicesAfterTestOperations(bp2)
Console.log2("r2: ", r2) // 2
