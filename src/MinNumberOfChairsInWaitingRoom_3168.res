// T(n) = O(n)
// S(n) = O(1)

let minNumberOfChairsInWaitingRoom = (people: string) => {
  let rec loop = (maxPeople: int, currentPeople: int, index: int) => {
    switch index === String.length(people) {
    | true => maxPeople
    | false => {
        let person = people->String.charAt(index)
        let newCurrent = person === "E" ? currentPeople + 1 : currentPeople - 1
        let newMax = newCurrent > maxPeople ? newCurrent : maxPeople

        loop(newMax, newCurrent, index + 1)
      }
    }
  }

  loop(Int32.min_int, 0, 0)
}

let s1 = "EEEEEEE"
let r1 = minNumberOfChairsInWaitingRoom(s1)
Console.log2("r1: ", r1) // 7

let s2 = "ELELEEL"
let r2 = minNumberOfChairsInWaitingRoom(s2)
Console.log2("r2: ", r2) // 2

let s3 = "ELEELEELLL"
let r3 = minNumberOfChairsInWaitingRoom(s3)
Console.log2("r3: ", r3) // 3
