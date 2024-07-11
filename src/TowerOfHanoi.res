// T(n) = O(2^n)
// S(n) = O(n)

// possibly incorrect
let towerOfHanoi = (startTower: array<int>) => {
  let amountOfDisks = Array.length(startTower)

  let moveOneDisk = (startTower, endTower): unit => {
    let disk = switch startTower->Array.pop {
    | None => Int32.min_int
    | Some(num) => num
    }
    endTower->Array.push(disk)
  }

  let rec loop = (
    amountOfDisks: int,
    ~startTower: array<int>,
    ~tempTower: array<int>,
    ~endTower: array<int>,
  ) => {
    switch amountOfDisks === 1 {
    | true => moveOneDisk(startTower, endTower)
    | false => {
        loop(amountOfDisks - 1, ~startTower, ~endTower=tempTower, ~tempTower=endTower)
        moveOneDisk(startTower, endTower)
        loop(amountOfDisks - 1, ~startTower, ~endTower, ~tempTower)
      }
    }
  }

  loop(amountOfDisks, ~startTower, ~tempTower=[], ~endTower=[])
}

let t1 = [1, 2, 3]
let r1 = towerOfHanoi(t1)
Console.log2("r1", r1)
