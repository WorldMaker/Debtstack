// Copyright 2012 Max Battcher. Some rights reserved.
// Licensed for use under the Ms-RL. See attached LICENSE file.
namespace Debtstack

open System

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