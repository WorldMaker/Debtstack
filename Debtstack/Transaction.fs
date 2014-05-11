// Copyright 2012 Max Battcher. Some rights reserved.
// Licensed for use under the Ms-RL. See attached LICENSE file.
namespace Debtstack

open System
open System.Windows

type TransactionType =
    | Adjustment = 0
    | Initial = 1
    | Interest = 2
    | InterestPaid = 3

type Transaction = {
    Key: int;
    Type: TransactionType;
    Name: string;
    Amount: decimal;
    Date: DateTime;
    }

type AccountType =
    | Debit = 0
    | Credit = 1
    | Interest = 2

type Account = {
    Type: AccountType;
    Initial: Transaction;
    Name: string;
    Category: string;
    Date: DateTime;
    Transactions: list<Transaction>;
    }
    with
        member this.Balance with get () = this.Transactions |> List.sumBy (fun tx -> tx.Amount)
        member this.TotalInterest with get () = this.Transactions |> List.filter (fun tx -> tx.Type = TransactionType.Interest) |> List.sumBy (fun tx -> tx.Amount)
        member this.PaidInterest with get () = this.Transactions |> List.filter (fun tx -> tx.Type = TransactionType.InterestPaid) |> List.sumBy (fun tx -> tx.Amount)
        member this.Interest with get () = this.TotalInterest + this.PaidInterest
        member this.PaidDate with get () = match this.Balance with
                                           | 0m -> Some this.Transactions.Head.Date
                                           | _ -> None
        member this.MonthAgo with get () = MonthAgo.monthAgo (this.Date)
        member this.CalendarSpan with get () = Option.bind (Some << CalendarSpan.calculate this.Date) this.PaidDate
        member this.MonthBetween with get () = Option.bind (Some << MonthAgo.monthBetween) this.CalendarSpan
        member this.InterestVisibility with get () = match this.TotalInterest with
                                                     | 0m -> Visibility.Collapsed
                                                     | _ -> Visibility.Visible
        member this.PaidMonthAgo with get () = match this.PaidDate with
                                               | Some date -> MonthAgo.monthAgo date
                                               | None -> "unpaid"

