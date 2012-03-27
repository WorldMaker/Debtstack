namespace Debtstack

open System
open ImpromptuInterface.MVVM
open ImpromptuInterface.FSharp

type TransactionType =
    Debit
    | Credit
    | Interest

type Transaction = {
    Type: TransactionType;
    Value: decimal;
    Date: DateTime;
    }

[<Interface>]
type ITransactionState =
    abstract member Transaction : Transaction with get, set
    abstract member Paid : decimal with get, set
    abstract member PaidDate : DateTime option with get, set

type TransactionState () as this =
    inherit ImpromptuViewModel<ITransactionState> ()

    do
        this.Contract.Paid <- 0m
        this.Contract.PaidDate <- None
        !?this.Dependencies?Transaction?Remaining?Link

    member this.Remaining with get () = match this.Contract.Transaction.Type with
        | Debit    -> this.Contract.Transaction.Value + this.Contract.Paid
        | Interest -> this.Contract.Transaction.Value + this.Contract.Paid
        | _        -> 0m