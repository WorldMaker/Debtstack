// Copyright 2012 Max Battcher. Some rights reserved.
// Licensed for use under the Ms-RL. See attached LICENSE file.
namespace Debtstack

open System
open System.Collections.ObjectModel
open System.ComponentModel
open System.IO
open System.Text.RegularExpressions
open System.Windows
open System.Windows.Data
open Microsoft.Win32
open ImpromptuInterface.FSharp
open ReactiveUI
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
type IBook =
    abstract member Current : Account with get, set
    abstract member Naive : Account with get, set
    abstract member Simple : Account with get, set
    abstract member Proportional : Account with get, set

type Book (initial : Account) as this =
    inherit Reflex<IBook> ()

    do
        this.Proxy.Current <- initial

    member this.ShowAccountDetails (arg: Object) =
        let acctWindow = Application.LoadComponent(new System.Uri("/Debtstack;component/accountwindow.xaml", UriKind.Relative)) :?> Window
        acctWindow.DataContext <- this.Proxy.Current
        acctWindow.Show ()

[<Interface>]
type IDebtstack =
    abstract member Books : ObservableCollection<Book> with get, set
    abstract member OpenBooks : CollectionViewSource with get, set
    abstract member ClosedBooks : CollectionViewSource with get, set
    abstract member IsLoading : bool with get, set

type Harness () as this =
    inherit Reflex<IDebtstack> ()

    let mutable Source = list<Account>.Empty
    let mutable Shelf = Map.empty<Transaction, Book>
    
    let mutable naiveComputed = false
    let mutable simpleComputed = false
    let mutable proportionalComputed = false

    do
        this.Proxy.IsLoading <- false

        this.Proxy.Books <- new ObservableCollection<Book> ()
        this.Proxy.OpenBooks <- new CollectionViewSource ()
        this.Proxy.OpenBooks.Source <- this.Proxy.Books
        this.Proxy.OpenBooks.Filter.Add (fun e -> e.Accepted <- match e.Item with
                                                                | :? Book as book -> book.Proxy.Current.PaidDate.IsNone
                                                                | _ -> false)
        this.Proxy.OpenBooks.SortDescriptions.Add (new SortDescription ("Current.Date", ListSortDirection.Ascending));
        this.Proxy.ClosedBooks <- new CollectionViewSource ()
        this.Proxy.ClosedBooks.Source <- this.Proxy.Books
        this.Proxy.ClosedBooks.Filter.Add (fun e -> e.Accepted <- match e.Item with
                                                                  | :? Book as book -> book.Proxy.Current.PaidDate.IsSome
                                                                  | _ -> false)
        this.Proxy.ClosedBooks.SortDescriptions.Add (new SortDescription ("Current.PaidDate", ListSortDirection.Descending));

        let loading = this.Proxy.WhenAnyValue (fun d -> d.IsLoading)
        this.React ("CanNaive", loading) |> ignore
        this.React ("CanSimple", loading) |> ignore
        this.React ("CanProportional", loading) |> ignore

    member this.Reset () =
        Source <- []
        this.Proxy.Books.Clear ()
        naiveComputed <- false
        simpleComputed <- false
        proportionalComputed <- false

    member this.TxCount with get () = Source.Length

    member this.TxSum with get () = this.Proxy.Books |> Seq.sumBy (fun book -> book.Proxy.Current.Balance)

    member this.TxInterest with get () = this.Proxy.Books |> Seq.sumBy (fun book -> book.Proxy.Current.Interest)

    member this.TxByCategory with get () = this.Proxy.Books |> Seq.filter (fun book -> book.Proxy.Current.PaidDate.IsNone)
                                                            |> Seq.groupBy (fun tx -> tx.Proxy.Current.Category)
                                                            |> Seq.map (fun (k, g) -> k, g |> Seq.map (fun book -> book.Proxy.Current.Balance) |> Seq.sum)

    member this.LoadTab (_ : obj) =
        this.Reset ()
        let to_acct = fun (line : string) -> match line.Split('\t') |> Array.filter (fun x -> x <> String.Empty) |> Array.toList with
                                             | d :: n :: a :: xs -> let amt = Handler.readAcct a
                                                                    let t = if n.StartsWith ("Interest", StringComparison.OrdinalIgnoreCase) then AccountType.Interest
                                                                            elif amt > 0m then AccountType.Credit
                                                                            else AccountType.Debit
                                                                    let tx = { Type = TransactionType.Initial; Name = n; Date = DateTime.Parse (d); Amount = amt; Key = 0; }
                                                                    Some { Type = t; Initial = tx; Name = n; Date = tx.Date; Category = String.Empty; Transactions = [tx] }
                                             | _ -> None

        let dialog = new OpenFileDialog ()
        let result = dialog.ShowDialog ()

        if result.HasValue && result.Value then Source <- File.ReadAllLines (dialog.FileName)
                                                |> Seq.filter (fun line -> line <> String.Empty)
                                                |> Seq.map    to_acct
                                                |> Seq.filter Option.isSome
                                                |> Seq.map    (fun x -> x.Value)
                                                |> List.ofSeq

                                                for acct in Source do
                                                    this.Proxy.Books.Add (new Book (acct))

                                                Shelf <- this.Proxy.Books |> Seq.map (fun book -> book.Proxy.Current.Initial, book) |> Map.ofSeq
        this.OnPropertyChanged "TxCount"

    member this.ReadMint (path : string) =
        let reader = new StreamReader (path)
        let csv = new CsvHelper.CsvReader (reader)
        let mutable i = 0
        while csv.Read () do
            if not (csv.["Category"].StartsWith ("Exclude", StringComparison.OrdinalIgnoreCase)) then
                let name = csv.["Description"]
                let t = if name.IndexOf ("Interest", StringComparison.OrdinalIgnoreCase) >= 0 then AccountType.Interest
                        else match csv.["Transaction Type"] with
                             | "debit"  -> AccountType.Debit
                             | "credit" -> AccountType.Credit
                             | _        -> raise (Handler.LoadProblem "Unknown transaction type")
                let amt = Decimal.Parse (csv.["Amount"])
                let value = if t = AccountType.Credit then amt else -amt
                let tx = { Type = TransactionType.Initial; Name = name; Date = DateTime.Parse (csv.["Date"]); Amount = value; Key = i; }
                let acct = { Type = t; Initial = tx; Name = name; Date = tx.Date; Category = csv.["Category"]; Transactions = [tx] }
                Source <- acct :: Source
                i <- i + 1

    member this.LoadMint (_ : obj) =
        this.Reset ()
        let dialog = new OpenFileDialog ()
        dialog.Filter <- "CSV Files|*.csv"
        let result = dialog.ShowDialog ()

        this.Proxy.IsLoading <- true
        let context = System.Threading.SynchronizationContext.Current
        async {
            do! Async.SwitchToThreadPool ()
            if result.HasValue && result.Value then this.ReadMint (dialog.FileName)                                                    

                                                    do! Async.SwitchToContext context
                                                    for acct in Source do
                                                        this.Proxy.Books.Add (new Book (acct))

                                                    do! Async.SwitchToThreadPool ()
                                                    Shelf <- this.Proxy.Books |> Seq.map (fun book -> book.Proxy.Current.Initial, book) |> Map.ofSeq
            do! Async.SwitchToContext context
            this.FinishedLoading ()
        } |> Async.Start

    member this.FinishedLoading () =
        this.Proxy.IsLoading <- false
        this.OnPropertyChanged "TxCount"

    member this.Naive () =
        if naiveComputed then for book in this.Proxy.Books do book.Proxy.Current <- book.Proxy.Naive
                         else let credits, debits = Strategies.naive Source
                              for acct in Seq.append debits credits do
                                  (Shelf.Item (acct.Initial)).Proxy.Naive <- acct
                                  (Shelf.Item (acct.Initial)).Proxy.Current <- acct
                              naiveComputed <- true
        this.Display ()

    member this.Simple (_ : obj) =
        if simpleComputed then for book in this.Proxy.Books do book.Proxy.Current <- book.Proxy.Simple
                          else let openaccts, closedaccts = Strategies.simpleStack Source
                               for acct in Seq.append openaccts closedaccts do
                                   (Shelf.Item (acct.Initial)).Proxy.Simple <- acct
                                   (Shelf.Item (acct.Initial)).Proxy.Current <- acct
                               simpleComputed <- true
        this.Display ()

    member this.Proportional (_ : obj) =
        if proportionalComputed then for book in this.Proxy.Books do book.Proxy.Current <- book.Proxy.Proportional
                                else let openaccts, closedaccts = Strategies.favorTheOld Source
                                     for acct in Seq.append openaccts closedaccts do
                                         (Shelf.Item (acct.Initial)).Proxy.Proportional <- acct
                                         (Shelf.Item (acct.Initial)).Proxy.Current <- acct
                                     proportionalComputed <- true
        this.Display ()

    member this.Display () =
        this.Proxy.OpenBooks.View.Refresh ()
        this.Proxy.ClosedBooks.View.Refresh ()
        this.OnPropertyChanged "TxSum"
        this.OnPropertyChanged "TxInterest"
        this.OnPropertyChanged "TxByCategory"
