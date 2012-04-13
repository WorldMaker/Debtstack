namespace Debtstack

open System
open System.Collections.ObjectModel
open System.IO
open System.Text.RegularExpressions
open Microsoft.Win32
open ImpromptuInterface.MVVM
open ImpromptuInterface.FSharp

module Handler =
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
    abstract member Transactions : ObservableCollection<TransactionState> with get, set

type Harness () as this =
    inherit ImpromptuViewModel<IDebtstack> ()

    do
        this.Contract.Transactions <- new ObservableCollection<TransactionState> ()

    member this.Load (_ : obj) =
        let to_tx = fun (line : string) -> match line.Split('\t') |> Array.filter (fun x -> x <> String.Empty) |> Array.toList with
                                           | d :: n :: a :: xs -> let amt = Handler.readAcct a
                                                                  let t = if n.StartsWith("Interest", StringComparison.OrdinalIgnoreCase) then Interest
                                                                          elif amt > 0m then Credit
                                                                          else Debit
                                                                  Some { Type = t; Name = n; Date = DateTime.Parse (d); Value = amt }
                                           | _ -> None

        let dialog = new OpenFileDialog ()
        let result = dialog.ShowDialog ()

        if result.HasValue && result.Value then File.ReadAllLines (dialog.FileName)
                                                |> Array.filter (fun line -> line <> String.Empty)
                                                |> Array.map (fun line -> match to_tx (line) with
                                                                          | Some tx -> this.Contract.Transactions.Add (new TransactionState (tx))
                                                                          | _ -> ())
                                                |> ignore

    member this.Reset (_ : obj) =
        Strategies.reset (this.Contract.Transactions |> Seq.toList)

    member this.Simple (_ : obj) =
        Strategies.simpleStack (this.Contract.Transactions |> Seq.toList)