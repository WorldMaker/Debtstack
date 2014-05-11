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

type Harness () as this =
    inherit Reflex<IDebtstack> ()

    let mutable Source = list<Account>.Empty
    let mutable Shelf = Map.empty<Transaction, Book>

    do
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

    member this.TxCount with get () = Source.Length

    member this.TxSum with get () = this.Proxy.Books |> Seq.sumBy (fun book -> book.Proxy.Current.Balance)

    member this.TxInterest with get () = this.Proxy.Books |> Seq.sumBy (fun book -> book.Proxy.Current.Interest)

    member this.TxByCategory with get () = this.Proxy.Books |> Seq.filter (fun book -> book.Proxy.Current.PaidDate.IsNone)
                                                            |> Seq.groupBy (fun tx -> tx.Proxy.Current.Category)
                                                            |> Seq.map (fun (k, g) -> k, g |> Seq.map (fun book -> book.Proxy.Current.Balance) |> Seq.sum)

    member this.LoadTab (_ : obj) =
        let to_acct = fun (line : string) -> match line.Split('\t') |> Array.filter (fun x -> x <> String.Empty) |> Array.toList with
                                             | d :: n :: a :: xs -> let amt = Handler.readAcct a
                                                                    let t = if n.StartsWith ("Interest", StringComparison.OrdinalIgnoreCase) then AccountType.Interest
                                                                            elif amt > 0m then AccountType.Credit
                                                                            else AccountType.Debit
                                                                    let tx = { Type = TransactionType.Initial; Name = n; Date = DateTime.Parse (d); Amount = amt; }
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

                                                this.Proxy.Books.Clear ()
                                                for acct in Source do
                                                    this.Proxy.Books.Add (new Book (acct))

                                                Shelf <- this.Proxy.Books |> Seq.map (fun book -> book.Proxy.Current.Initial, book) |> Map.ofSeq
        this.OnPropertyChanged "TxCount"

    member this.LoadMint (_ : obj) =
        let dialog = new OpenFileDialog ()
        dialog.Filter <- "CSV Files|*.csv"
        let result = dialog.ShowDialog ()

        if result.HasValue && result.Value then let reader = new StreamReader (dialog.OpenFile ())
                                                let csv = new CsvHelper.CsvReader (reader)
                                                Source <- []
                                                this.Proxy.Books.Clear ()
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
                                                    let tx = { Type = TransactionType.Initial; Name = name; Date = DateTime.Parse (csv.["Date"]); Amount = value; }
                                                    let acct = { Type = t; Initial = tx; Name = name; Date = tx.Date; Category = csv.["Category"]; Transactions = [tx] }
                                                    Source <- acct :: Source

                                                for acct in Source do
                                                    this.Proxy.Books.Add (new Book (acct))

                                                Shelf <- this.Proxy.Books |> Seq.map (fun book -> book.Proxy.Current.Initial, book) |> Map.ofSeq
        this.OnPropertyChanged "TxCount"

    member this.Simple (_ : obj) =
        let openaccts, closedaccts = Strategies.simpleStack Source
        for acct in Seq.append openaccts closedaccts do
            (Shelf.Item (acct.Initial)).Proxy.Simple <- acct
            (Shelf.Item (acct.Initial)).Proxy.Current <- acct
        this.Display ()

    member this.Proportional (_ : obj) =
        let openaccts, closedaccts = Strategies.favorTheOld Source
        for acct in Seq.append openaccts closedaccts do
            (Shelf.Item (acct.Initial)).Proxy.Proportional <- acct
            (Shelf.Item (acct.Initial)).Proxy.Current <- acct
        this.Display ()

    member this.Display () =
        this.Proxy.OpenBooks.View.Refresh ()
        this.Proxy.ClosedBooks.View.Refresh ()
        this.OnPropertyChanged "TxSum"
        this.OnPropertyChanged "TxInterest"
        this.OnPropertyChanged "TxByCategory"
