// T(n) = O(n)
// S(n) = O(n)

let timeNeededToBuyTickets = (ticketsToBuy: array<int>, position: int) => {
  let rec loop = (tickets: array<int>, elapsedTime: int, index: int) => {
    let ticketWanting = switch tickets->Array.at(index) {
    | None => -1
    | Some(n) => n
    }

    switch index === position && ticketWanting === 1 {
    | true => elapsedTime + 1
    | false =>
      switch index === Array.length(ticketsToBuy) {
      | true => ticketWanting < 0 ? loop(tickets, elapsedTime, 0) : elapsedTime
      | false =>
        ticketWanting === 0
          ? loop(tickets, elapsedTime, index + 1)
          : loop(
              tickets->Array.mapWithIndex((t, idx) => idx === index ? t - 1 : t),
              elapsedTime + 1,
              index + 1,
            )
      }
    }
  }

  loop(ticketsToBuy, 0, 0)
}

let t1 = [2, 3, 2]
let p1 = 2
let r1 = timeNeededToBuyTickets(t1, p1)
Console.log2("r1: ", r1) // 6

let t2 = [5, 1, 1, 1]
let p2 = 0
let r2 = timeNeededToBuyTickets(t2, p2)
Console.log2("r2: ", r2) // 8
