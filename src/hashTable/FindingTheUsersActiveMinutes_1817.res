// T(n) = O(n)
// S(n) = O(n)

let findingTheUsersActiveMinutes = (logs: array<(int, int)>, k: int) => {
  let rec makeIdActionsTable = (idActionsTable: Map.t<int, Set.t<int>>, index: int) => {
    switch index === Array.length(logs) {
    | true => idActionsTable
    | false => {
        let (id, minute) = switch logs->Array.at(index) {
        | None => (-1, -1)
        | Some(t) => t
        }
        let actions = switch idActionsTable->Map.get(id) {
        | None => Set.make()
        | Some(set) => set
        }
        actions->Set.add(minute)
        idActionsTable->Map.set(id, actions)

        makeIdActionsTable(idActionsTable, index + 1)
      }
    }
  }

  let rec makeActionsUsersCountTable = (
    actionsUsersCountTable: Map.t<int, int>,
    idActionsTuples: array<(int, Set.t<int>)>,
    index: int,
  ) => {
    switch index === Array.length(idActionsTuples) {
    | true => actionsUsersCountTable
    | false => {
        let (_id, actions) = switch idActionsTuples->Array.at(index) {
        | None => (-1, Set.make())
        | Some(t) => t
        }
        let size = Set.size(actions)
        let count = switch actionsUsersCountTable->Map.get(size) {
        | None => 1
        | Some(c) => c + 1
        }
        actionsUsersCountTable->Map.set(size, count)

        makeActionsUsersCountTable(actionsUsersCountTable, idActionsTuples, index + 1)
      }
    }
  }

  let idActionsTuples = makeIdActionsTable(Map.make(), 0)->Map.entries->Array.fromIterator
  let actionsUsersCountTable = makeActionsUsersCountTable(Map.make(), idActionsTuples, 0)

  let rec fillAnswers = (
    answers: array<int>,
    actionsUsersCountTuples: array<(int, int)>,
    index: int,
  ) => {
    switch index === Array.length(actionsUsersCountTuples) {
    | true => answers
    | false => {
        let (actions, count) = switch actionsUsersCountTuples->Array.at(index) {
        | None => (-1, -1)
        | Some(t) => t
        }

        fillAnswers(
          answers->Array.mapWithIndex((n, idx) => idx + 1 === actions ? count : n),
          actionsUsersCountTuples,
          index + 1,
        )
      }
    }
  }

  fillAnswers(Array.make(~length=k, 0), actionsUsersCountTable->Map.entries->Array.fromIterator, 0)
}

let l1 = [(0, 5), (1, 2), (0, 2), (0, 5), (1, 3)]
let k1 = 5
let r1 = findingTheUsersActiveMinutes(l1, k1)
Console.log2("r1: ", r1) // [0,2,0,0,0]

let l2 = [(1, 1), (2, 2), (2, 3)]
let k2 = 4
let r2 = findingTheUsersActiveMinutes(l2, k2)
Console.log2("r2: ", r2) // [1,1,0,0]
