// Copyright 2012 Max Battcher. Some rights reserved.
// Licensed for use under the Ms-RL. See attached LICENSE file.
namespace Debtstack

open System
open System.Collections.ObjectModel
open System.IO
open System.Text.RegularExpressions
open Microsoft.Win32
open ImpromptuInterface.FSharp
open ReflexUX

module Handler =
    exception LoadProblem of string

    let (|Match|_|) pattern input =
      let re = new Regex (pattern, RegexOptions.Compiled)
      let m = re.Match input
      if m.Success then Some (re.GetGroupNames ()
                              |> Seq.map (fun n -> (n, m.Groups.[n]))
                              |> Seq.filter (fun (n, g) -> g.Success)
                              |> Seq.map (fun (n, g) -> (n, g.Value))
                              |> Map.ofSeq)
      else None

    let readAcct = function
                   | Match "\$\((?<amt>[\d\,]+(\.\d+))\)" result -> -(Decimal.Parse result.["amt"])
                   | Match "\$(?<amt>\-?[\d\,]+(\.\d+))" result -> Decimal.Parse result.["amt"]
                   | input -> Decimal.Parse input

[<Interface>]
type IDebtstack =
    abstract member Transactions     : ObservableCollection<TransactionState> with get, set
    abstract member PaidTransactions : ObservableCollection<TransactionState> with get, set

type Harness () as this =
    inherit Reflex<IDebtstack> ()

    let mutable Source = list<TransactionState>.Empty

    do
        this.Proxy.Transactions     <- new ObservableCollection<TransactionState> ()
        this.Proxy.PaidTransactions <- new ObservableCollection<TransactionState> ()

    member this.TxCount with get () = Source.Length

    member this.TxSum with get () = this.Proxy.Transactions |> Seq.sumBy (fun tx -> tx.TrueRemaining)

    member this.TxSimpleSum with get () = Source |> Seq.sumBy (fun tx -> tx.Proxy.Transaction.Value)

    member this.TxInterest with get () = Source |> Seq.sumBy (fun tx -> tx.Proxy.Interest)

    member this.TxByCategory with get () = this.Proxy.Transactions |> Seq.groupBy (fun tx -> tx.Proxy.Transaction.Category)
                                                                   |> Seq.map (fun (k, g) -> (k, g |> Seq.map (fun tx -> tx.TrueRemaining) |> Seq.sum))

    member this.LoadTab (_ : obj) =
        let to_tx = fun (line : string) -> match line.Split('\t') |> Array.filter (fun x -> x <> String.Empty) |> Array.toList with
                                           | d :: n :: a :: xs -> let amt = Handler.readAcct a
                                                                  let t = if n.StartsWith ("Interest", StringComparison.OrdinalIgnoreCase) then Interest
                                                                          elif amt > 0m then Credit
                                                                          else Debit
                                                                  Some { Type = t; Name = n; Date = DateTime.Parse (d); Value = amt; Category = String.Empty; }
                                           | _ -> None

        let dialog = new OpenFileDialog ()
        let result = dialog.ShowDialog ()

        if result.HasValue && result.Value then Source <- File.ReadAllLines (dialog.FileName)
                                                |> Array.filter (fun line -> line <> String.Empty)
                                                |> Array.map    to_tx
                                                |> Array.filter Option.isSome
                                                |> Array.map    (fun tx -> new TransactionState (tx.Value))
                                                |> List.ofArray
        this.OnPropertyChanged "TxCount"
        this.OnPropertyChanged "TxSimpleSum"

    member this.LoadMint (_ : obj) =
        let dialog = new OpenFileDialog ()
        dialog.Filter <- "CSV Files|*.csv"
        let result = dialog.ShowDialog ()

        if result.HasValue && result.Value then let reader = new StreamReader (dialog.OpenFile ())
                                                let csv = new CsvHelper.CsvReader (reader)
                                                Source <- []
                                                while csv.Read () do
                                                  if not (csv.["Category"].StartsWith ("Exclude", StringComparison.OrdinalIgnoreCase)) then
                                                    let name = csv.["Description"]
                                                    let t = if name.IndexOf ("Interest", StringComparison.OrdinalIgnoreCase) >= 0 then Interest
                                                            else match csv.["Transaction Type"] with
                                                                 | "debit"  -> Debit
                                                                 | "credit" -> Credit
                                                                 | _        -> raise (Handler.LoadProblem "Unknown transaction type")
                                                    let amt = Decimal.Parse (csv.["Amount"])
                                                    let value = if t = Credit then amt else -amt
                                                    let tx = new TransactionState ({ Type = t; Name = name; Date = DateTime.Parse (csv.["Date"]); Value = value; Category = csv.["Category"]; })
                                                    Source <- tx :: Source
        this.OnPropertyChanged "TxCount"
        this.OnPropertyChanged "TxSimpleCount"

    member this.Reset (_ : obj) =
        this.Proxy.Transactions.Clear ()
        this.Proxy.PaidTransactions.Clear ()
        Strategies.reset Source

    member this.Simple (_ : obj) =
        Strategies.simpleStack Source
        this.Display ()

    member this.Proportional (_ : obj) =
        Strategies.favorTheOld Source
        this.Display ()

    member this.Display () =
        this.Proxy.Transactions.Clear ()
        this.Proxy.PaidTransactions.Clear ()
        Source |> List.filter (fun tx -> tx.TrueRemaining <> 0m)
               |> List.map    this.Proxy.Transactions.Add
               |> ignore
        Source |> List.filter (fun tx -> tx.Proxy.PaidDate.IsSome)
               |> List.sortBy (fun tx -> (tx.Proxy.PaidDate.Value, tx.Proxy.Transaction.Value))
               |> List.rev
               |> List.map    this.Proxy.PaidTransactions.Add
               |> ignore
        this.OnPropertyChanged "TxSum"
        this.OnPropertyChanged "TxInterest"
        this.OnPropertyChanged "TxByCategory"