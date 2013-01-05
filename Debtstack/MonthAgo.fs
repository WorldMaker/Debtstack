// Copyright 2012 Max Battcher. Some rights reserved.
// Licensed for use under the Ms-RL. See attached LICENSE file.
namespace Debtstack

open System

type CalendarSpan =
   { Years : int; Months : int; }
   static member calculate (startDate : DateTime) (endDate : DateTime) =
      let totalmonths = endDate.Month - startDate.Month + (endDate.Year - startDate.Year) * 12
      let years = totalmonths / 12
      { Years = years; Months = totalmonths - years * 12 }

module MonthAgo =
    let monthAgo (date : DateTime) =
        let now = DateTime.Now
        let months = (now.Year * 12 + now.Month) - (date.Year * 12 + date.Month)
        match months with
            | -1 -> "next month"
            | 0 -> "this month"
            | 1 -> "a month ago"
            | x when x > 1 && x < 12 -> sprintf "%A months ago" x
            | x when x >= 12 && x < 24 -> "a year ago"
            | x when x >= 24 -> sprintf "%A years ago" (x / 12)
            | x when x < -1 -> "the future"
            | _ -> "elsewhen"

    let monthBetween (cs : CalendarSpan) =
        match cs with
            | { Years = 0; Months = 0; } -> "that month"
            | { Years = 0; Months = 1; } -> "a month"
            | { Years = 0; Months = m; } -> sprintf "%A months" m
            | { Years = 1; Months = 0; } -> "a year"
            | { Years = 1; Months = 1; } -> "a year and a month"
            | { Years = 1; Months = m; } -> sprintf "a year and %A months" m
            | { Years = y; Months = 0; } -> sprintf "%A years" y
            | { Years = y; Months = 1; } -> sprintf "%A years and a month" y
            | { Years = y; Months = m; } -> sprintf "%A years and %A months" y m