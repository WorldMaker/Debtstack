namespace Debtstack

open System
open System.Collections.ObjectModel
open ImpromptuInterface.MVVM
open ImpromptuInterface.FSharp

[<Interface>]
type IDebtstack =
    abstract member Transactions : ObservableCollection<TransactionState> with get, set

type Harness () as this =
    inherit ImpromptuViewModel<IDebtstack> ()

    do
        this.Contract.Transactions <- new ObservableCollection<TransactionState> ()

    member this.Load (_ : obj) =
        this.Contract.Transactions.Add (new TransactionState ({ Type = Debit; Name = "Jim's Crab Bungalow"; Value = 13.95m; Date = DateTime.Today.AddDays(-3.0); }))