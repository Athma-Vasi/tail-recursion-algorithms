// INCORRECT

let minimumTimeDifference = (timePoints: array<string>): int => {
  let length = Array.length(timePoints)
  let clone = timePoints->Array.map(num => num)
  clone->Array.sort((a, b) => {
    let compareFloat = String.localeCompare(a, b)
    compareFloat < 0.0 ? -1.0 : compareFloat > 0.0 ? 1.0 : 0.0
  })

  open Console
  log2("clone after sorting", clone)

  let rec loop = (diff: int, index: int) => {
    let leftTimePoint = switch timePoints->Array.get(index) {
    | None => ""
    | Some(time) => time
    }
    let leftSplitTime = String.split(leftTimePoint, ":")
    let leftHours = switch leftSplitTime->Array.get(0) {
    | None => ""
    | Some(hrs) => hrs
    }
    let leftMinutes = switch leftSplitTime->Array.get(1) {
    | None => ""
    | Some(mins) => mins
    }
    let leftHoursFloat = switch leftHours->Int.fromString {
    | None => Int32.min_int
    | Some(hoursFloat) => hoursFloat
    }->Int.toFloat
    let leftMinutesFloat = switch leftMinutes->Int.fromString {
    | None => Int32.min_int
    | Some(minutesFloat) => minutesFloat
    }->Int.toFloat
    let leftTotalSeconds = leftHoursFloat *. 60.0 +. leftMinutesFloat

    Console.log2("leftTotalSeconds", leftTotalSeconds)

    //
    //
    //

    let rightTimePoint = switch timePoints->Array.get(index + 1) {
    | None => ""
    | Some(time) => time
    }
    let rightSplitTime = String.split(rightTimePoint, ":")
    let rightHours = switch rightSplitTime->Array.get(0) {
    | None => ""
    | Some(hrs) => hrs
    }
    let rightMinutes = switch rightSplitTime->Array.get(1) {
    | None => ""
    | Some(mins) => mins
    }
    let rightHoursFloat = switch rightHours->Int.fromString {
    | None => Int32.min_int
    | Some(hoursFloat) => hoursFloat
    }->Int.toFloat
    let rightMinutesFloat = switch rightMinutes->Int.fromString {
    | None => Int32.min_int
    | Some(minutesFloat) => minutesFloat
    }->Int.toFloat
    let rightTotalSeconds = rightHoursFloat *. 60.0 +. rightMinutesFloat

    Console.log2("rightTotalSeconds", rightTotalSeconds)

    // index === length - 2 ? smallestDiff : loop(smallestDiff, index + 1)
    switch index === length - 2 {
    | true => diff
    | false =>
      switch leftTotalSeconds === 0.0 {
      | true => {
          let midnightDiff = Math.abs(rightTotalSeconds -. 1440.0)
          let zeroDiff = Math.abs(rightTotalSeconds -. 0.0)

          switch midnightDiff < zeroDiff {
          | true => loop(Float.toInt(midnightDiff), index + 1)
          | false => loop(Float.toInt(zeroDiff), index + 1)
          }
        }
      | false =>
        switch rightTotalSeconds === 0.0 {
        | true => {
            let midnightDiff = Math.abs(1440.0 -. leftTotalSeconds)
            let zeroDiff = Math.abs(0.0 -. leftTotalSeconds)

            switch midnightDiff < zeroDiff {
            | true => loop(Float.toInt(midnightDiff), index + 1)
            | false => loop(Float.toInt(zeroDiff), index + 1)
            }
          }
        | false => {
            let currentDiff = rightTotalSeconds -. leftTotalSeconds
            switch Float.toInt(currentDiff) < diff {
            | true => loop(Float.toInt(currentDiff), index + 1)
            | false => loop(diff, index + 1)
            }
          }
        }
      }
    }
  }

  loop(Int32.max_int, 0)
}

let timePoints1 = ["23:59", "00:00"]
let result1 = minimumTimeDifference(timePoints1)
Console.log2("result1", result1)

let timePoints2 = ["00:00", "23:59", "00:00"]
let result2 = minimumTimeDifference(timePoints2)
Console.log2("result2", result2)
