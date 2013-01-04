// Copyright 2012 Max Battcher. Some rights reserved.
// Licensed for use under the Ms-RL. See attached LICENSE file.
namespace Debtstack

open System
open System.Windows
open ImpromptuInterface.MVVM
open ImpromptuInterface.FSharp

type TransactionType =
    Debit
    | Credit
    | Interest

type Transaction = {
    Type: TransactionType;
    Name: string;
    Value: decimal;
    Date: DateTime;
    Category: string;
    }

[<Interface>]
type ITransactionState =
    abstract member Transaction : Transaction with get, set
    abstract member Paid : decimal with get, set
    abstract member PaidDate : DateTime option with get, set
    abstract member Interest : decimal with get, set
    abstract member TotalInterest : decimal with get, set

type TransactionState (tx : Transaction) as this =
    inherit ImpromptuViewModel<ITransactionState> ()

    do
        this.Contract.Transaction <- tx
        this.Contract.Paid <- 0m
        this.Contract.PaidDate <- None
        this.Dependencies?Interest?Remaining?Link ()
        this.Dependencies?Transaction?Remaining?Link ()
        this.Dependencies?Transaction?MonthAgo?Link ()
        this.Dependencies?PaidDate?PaidMonthAgo?Link ()
        this.Dependencies?TotalInterest?InterestVisibility?Link ()

    member this.Remaining with get () = match this.Contract.Transaction.Type with
                                        | Debit    -> this.Contract.Transaction.Value + this.Contract.Interest + this.Contract.Paid
                                        | _        -> 0m

    member this.InterestVisibility with get () = if this.Contract.TotalInterest <> 0m then Visibility.Visible else Visibility.Collapsed

    member this.MonthAgo with get () = MonthAgo.monthAgo this.Contract.Transaction.Date

    member this.PaidMonthAgo with get () = if this.Contract.PaidDate.IsSome then MonthAgo.monthAgo this.Contract.PaidDate.Value else "unpaid"

    member this.Reset () =
        this.Contract.Paid <- 0m
        this.Contract.PaidDate <- None
        this.Contract.Interest <- 0m
        this.Contract.TotalInterest <- 0m

    member this.Interest interest = this.Contract.Interest      <- this.Contract.Interest      + interest
                                    this.Contract.TotalInterest <- this.Contract.TotalInterest + interest

    member this.Pay credit date = let intamt = min -this.Contract.Interest credit
                                  this.Contract.Interest <- this.Contract.Interest - intamt
                                  let amt = min -this.Remaining (credit - intamt)
                                  if amt > 0m && this.Remaining < 0m then this.Contract.Paid <- this.Contract.Paid + amt
                                                                          this.Contract.PaidDate <- if this.Remaining = 0m then Some date else None
                                                                          amt
                                                                     else 0m