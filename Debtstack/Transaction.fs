// Copyright 2012 Max Battcher. Some rights reserved.
// Licensed for use under the Ms-RL. See attached LICENSE file.
namespace Debtstack

open System
open System.Windows
open ImpromptuInterface.FSharp
open ReactiveUI
open ReflexUX

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
    inherit Reflex<ITransactionState> ()

    let calculateRemaining (interest : decimal) (paid : decimal) (tx : Transaction) = match tx.Type with
                                                                                      | Debit -> tx.Value + interest + paid
                                                                                      | _     -> 0m

    do
        this.Proxy.Interest <- 0m
        this.Proxy.Transaction <- tx
        this.Proxy.Paid <- 0m
        this.Proxy.PaidDate <- None
        let remaining = WhenAnyMixin.WhenAny (this.Proxy, (fun x -> x.Interest), (fun x -> x.Paid), (fun x -> x.Transaction), (fun a b c -> calculateRemaining a.Value b.Value c.Value))
        this.React ("Remaining", remaining) |> ignore
        let txMonthAgo = WhenAnyMixin.WhenAny (this.Proxy, (fun x -> x.Transaction), (fun tx -> MonthAgo.monthAgo tx.Value.Date))
        this.React ("MonthAgo", txMonthAgo) |> ignore
        let txCalSpan = WhenAnyMixin.WhenAny ( this.Proxy
                                             , (fun x -> x.Transaction)
                                             , (fun x -> x.PaidDate)
                                             , (fun tx paidDate -> Option.bind (fun x -> Some (CalendarSpan.calculate tx.Value.Date x)) paidDate.Value)
                                             )
        let cspan = this.React ("CalendarSpan", txCalSpan)
        this.React ("MonthBetween", cspan |> Observable.map (Option.bind (fun x -> Some (MonthAgo.monthBetween x)))) |> ignore
        let intVis = WhenAnyMixin.WhenAny (this.Proxy, (fun x -> x.TotalInterest), (fun ti -> if ti.Value <> 0m then Visibility.Visible else Visibility.Collapsed))
        this.React ("InterestVisibility", intVis) |> ignore
        let pdMonthAgo = WhenAnyMixin.WhenAny (this.Proxy
                                              , (fun x -> x.PaidDate)
                                              , (fun pd -> if pd.Value.IsSome then MonthAgo.monthAgo pd.Value.Value else "unpaid")
                                              )
        this.React ("PaidMonthAgo", pdMonthAgo) |> ignore

    member this.TrueRemaining with get () = calculateRemaining this.Proxy.Interest this.Proxy.Paid this.Proxy.Transaction // TODO: Is this a hacK?

    member this.Reset () =
        this.Proxy.Paid <- 0m
        this.Proxy.PaidDate <- None
        this.Proxy.Interest <- 0m
        this.Proxy.TotalInterest <- 0m

    member this.Interest interest = this.Proxy.Interest      <- this.Proxy.Interest      + interest
                                    this.Proxy.TotalInterest <- this.Proxy.TotalInterest + interest

    member this.Pay credit date = let intamt = min -this.Proxy.Interest credit
                                  this.Proxy.Interest <- this.Proxy.Interest + intamt
                                  let amt = min -this.TrueRemaining (credit - intamt)
                                  if amt > 0m && this.TrueRemaining < 0m then this.Proxy.Paid <- this.Proxy.Paid + amt
                                                                              this.Proxy.PaidDate <- if this.TrueRemaining = 0m then Some date else None
                                                                              amt + intamt
                                                                         else intamt