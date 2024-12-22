// T(n) = O(n)
// S(n) = O(1)

let numberOfEmployeesWhoMeetTarget = (hours: array<int>, target: int) => {
  let rec countEmployees = (amount: int, index: int) => {
    switch index === Array.length(hours) {
    | true => amount
    | false => {
        let hour = hours->Array.at(index)->Option.mapOr(-1, h => h)
        countEmployees(hour >= target ? amount + 1 : amount, index + 1)
      }
    }
  }

  countEmployees(0, 0)
}

let h1 = [0, 1, 2, 3, 4]
let t1 = 2
let r1 = numberOfEmployeesWhoMeetTarget(h1, t1)
Console.log2("r1: ", r1) // 3

let h2 = [5, 1, 4, 2, 2]
let t2 = 6
let r2 = numberOfEmployeesWhoMeetTarget(h2, t2)
Console.log2("r2: ", r2) // 0
