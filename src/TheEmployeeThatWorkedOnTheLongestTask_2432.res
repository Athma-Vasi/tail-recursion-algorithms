// T(n) = O(n)
// S(n) = O(1)

let theEmployeeThatWorkedOnTheLongestTask = (_n: int, logs: array<(int, int)>) => {
  let rec find = (id: int, longest: int, prevTask: int, index: int) => {
    switch index === Array.length(logs) {
    | true => id
    | false => {
        let (currId, currTask) = logs->Array.at(index)->Option.mapOr((-1, -1), t => t)
        let currTime = currTask - prevTask

        switch currTime > longest {
        | true => find(currId, currTime, currTask, index + 1)
        | false =>
          switch currTime === longest {
          | true => find(currId < id ? currId : id, longest, currTask, index + 1)
          | false => find(id, longest, currTask, index + 1)
          }
        }
      }
    }
  }

  let (firstId, firstTask) = logs->Array.at(0)->Option.mapOr((-1, -1), t => t)
  find(firstId, firstTask, firstTask, 1)
}

let n1 = 10
let l1 = [(0, 3), (2, 5), (0, 9), (1, 15)]
let r1 = theEmployeeThatWorkedOnTheLongestTask(n1, l1)
Console.log2("r1: ", r1) // 1

let n2 = 26
let l2 = [(1, 1), (3, 7), (2, 12), (7, 17)]
let r2 = theEmployeeThatWorkedOnTheLongestTask(n2, l2)
Console.log2("r2: ", r2) // 3

let n3 = 2
let l3 = [(0, 10), (1, 20)]
let r3 = theEmployeeThatWorkedOnTheLongestTask(n3, l3)
Console.log2("r3: ", r3) // 0
