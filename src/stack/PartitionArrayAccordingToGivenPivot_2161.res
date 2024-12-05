let partitionArrayAccordingToGivenPivot = (nums: array<int>, pivot: int) => {
  let rec partition = (
    lesserStack: array<int>,
    equalStack: array<int>,
    greaterStack: array<int>,
    index: int,
  ) => {
    switch index === Array.length(nums) {
    | true => lesserStack->Array.concat(equalStack)->Array.concat(greaterStack)
    | false => {
        let num = switch nums->Array.at(index) {
        | None => Int32.min_int
        | Some(n) => n
        }

        switch num < pivot {
        | true => partition(lesserStack->Array.concat([num]), equalStack, greaterStack, index + 1)
        | false =>
          switch num === pivot {
          | true => partition(lesserStack, equalStack->Array.concat([num]), greaterStack, index + 1)
          | false =>
            partition(lesserStack, equalStack, greaterStack->Array.concat([num]), index + 1)
          }
        }
      }
    }
  }

  partition([], [], [], 0)
}

let n1 = [9, 12, 5, 10, 14, 3, 10]
let p1 = 10
let r1 = partitionArrayAccordingToGivenPivot(n1, p1)
Console.log2("r1: ", r1) // [9, 5, 3, 10, 10, 12, 14]

let n2 = [-3, 4, 3, 2]
let p2 = 2
let r2 = partitionArrayAccordingToGivenPivot(n2, p2)
Console.log2("r2: ", r2) // [-3, 2, 4 ,3]
