module Transaction

open System

type TransactionType =
    Debit
    | Credit
    | Interest

type Transaction = {
    Type: TransactionType;
    Value: decimal;
    mutable Paid: decimal;
    Date: DateTime;
    }
    with
        member this.Remaining with get() = match this.Type with
            | Debit -> this.Value + this.Paid
            | Interest -> this.Value + this.Paid
            | _ -> decimal 0