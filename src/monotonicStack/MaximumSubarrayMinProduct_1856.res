let maximumSubarrayMinProduct = (nums: array<int>) => {
  let rec updateMonoIncrStack = (
    ~indexToPush: int,
    ~max: int,
    ~monoIncrStack: array<int>,
    ~sum: int,
  ) => {
    let numToPush = switch nums->Array.at(indexToPush) {
    | None => -1
    | Some(n) => n
    }
    let prevIdx = switch monoIncrStack->Array.at(-1) {
    | None => -1
    | Some(n) => n
    }
    let prevNum = switch nums->Array.at(prevIdx) {
    | None => Int32.min_int
    | Some(n) => n
    }

    switch numToPush > prevNum {
    | true => updateMonoIncrStack(~indexToPush, ~max, ~monoIncrStack, ~sum=sum + numToPush)
    | false => {
        let newMax = numToPush * (sum - max)
      }
    }
  }
}
