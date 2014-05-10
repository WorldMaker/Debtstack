// Copyright 2012 Max Battcher. Some rights reserved.
// Licensed for use under the Ms-RL. See attached LICENSE file.
namespace Debtstack

open ImpromptuInterface.FSharp

module Strategies =
    exception MoneyProblems of string

    let roundpenny money = truncate (money * 100m) / 100m

    let adjust acct tx = { acct with Transactions = tx :: acct.Transactions }

    let pay = function
        | from, (acct : Account), amt when acct.Interest < 0m && abs acct.Interest < amt -> let txs = { Type = TransactionType.Adjustment; Name = from.Name; Amount = amt + acct.Interest; Date = from.Date }
                                                                                                    :: { Type = TransactionType.InterestPaid; Name = from.Name; Amount = -acct.Interest; Date = from.Date }
                                                                                                    :: acct.Transactions
                                                                                            { acct with Transactions = txs }
        | from, (acct : Account), amt when acct.Interest < 0m -> adjust acct { Type = TransactionType.InterestPaid; Name = from.Name; Amount = amt; Date = from.Date }
        | from, (acct : Account), amt -> adjust acct { Type = TransactionType.Adjustment; Name = from.Name; Amount = amt; Date = from.Date }

    let simpleStack (accts : list<Account>) =
        let rec payback = function
                        | (from : Account), (next : Account) :: more, results 
                          when from.Balance > 0m && next.Balance < 0m -> let bal = abs next.Balance
                                                                         let amt = min from.Balance bal
                                                                         payback (adjust from { Type = TransactionType.Adjustment; Name = next.Name; Amount = -amt; Date = from.Date }
                                                                            , more
                                                                            , pay (from, next, amt) :: results)
                        | (from : Account), (next : Account) :: more, results -> payback (from, more, next :: results)
                        | from, [], results when from.Balance > 0m -> raise (MoneyProblems "Payback failed")
                        | from, [], results -> from, results

        let rec interesting = function
                | (from : Account), amt, (next : Account) :: more, results 
                    when from.Balance < 0m -> interesting (adjust from { Type = TransactionType.Adjustment; Name = next.Name; Amount = -amt; Date = from.Date }
                                                          , amt
                                                          , more
                                                          , adjust next { Type = TransactionType.Interest; Name = from.Name; Amount = amt; Date = from.Date } :: results)
                | (from : Account), amt, (next : Account) :: more, results -> payback (from, more, next :: results)
                | from, amt, next :: [], results -> (adjust from { Type = TransactionType.Adjustment; Name = next.Name; Amount = -from.Balance; Date = from.Date }
                                                    , adjust next { Type = TransactionType.Interest; Name = from.Name; Amount = from.Balance; Date = from.Date } :: results)
                | from, _, [], results -> from, results

        let rec screen = function
                       | (acct : Account) :: accts, stack, results -> match acct.Type with
                                                                      | Debit    -> screen (accts, acct :: stack, results)
                                                                      | Interest -> let amt = roundpenny (acct.Balance / (decimal stack.Length))
                                                                                    let acct, stack = interesting (acct, amt, stack, [])
                                                                                    screen (accts, List.rev stack, acct :: results)
                                                                      | Credit   -> let acct, stack = payback (acct, stack, [])
                                                                                    screen (accts
                                                                                           , stack |> List.filter (fun x -> x.PaidDate.IsNone) |> List.rev
                                                                                           , acct :: (stack |> List.filter (fun x -> x.PaidDate.IsSome) |> List.append results))
                       | [], stack, results                        -> stack, results
        screen (accts, [], [])

    let favorTheOld (accts : list<Account>) =
        let rec payback = function
                        | (from : Account), (next : Account) :: more, results 
                          when from.Balance > 0m && next.Balance < 0m -> let bal = abs next.Balance
                                                                         let amt = min (roundpenny (from.Balance / (decimal more.Length + 1m))) bal
                                                                         payback (adjust from { Type = TransactionType.Adjustment; Name = next.Name; Amount = -amt; Date = from.Date }
                                                                            , more
                                                                            , pay (from, next, amt) :: results)
                        | (from : Account), (next : Account) :: more, results -> payback (from, more, next :: results)
                        // | from, [], results when from.Balance > 0m -> raise (MoneyProblems "Payback failed")
                        | from, [], results -> from, results

        let rec interesting = function
                | (from : Account), amt, (next : Account) :: more, results 
                    when from.Balance < 0m -> interesting (adjust from { Type = TransactionType.Adjustment; Name = next.Name; Amount = -amt; Date = from.Date }
                                                          , amt
                                                          , more
                                                          , adjust next { Type = TransactionType.Interest; Name = from.Name; Amount = amt; Date = from.Date } :: results)
                | (from : Account), amt, (next : Account) :: more, results -> payback (from, more, next :: results)
                | from, amt, next :: [], results -> (adjust from { Type = TransactionType.Adjustment; Name = next.Name; Amount = -from.Balance; Date = from.Date }
                                                    , adjust next { Type = TransactionType.Interest; Name = from.Name; Amount = from.Balance; Date = from.Date } :: results)
                | from, _, [], results -> from, results

        let rec screen = function
                       | (acct : Account) :: accts, stack, results -> match acct.Type with
                                                                      | Debit    -> screen (accts, acct :: stack, results)
                                                                      | Interest -> let amt = roundpenny (acct.Balance / (decimal stack.Length))
                                                                                    let acct, stack = interesting (acct, amt, stack, [])
                                                                                    screen (accts, List.rev stack, acct :: results)
                                                                      | Credit   -> let mutable curAcct, curStack, curResults = acct, stack, results
                                                                                    while curAcct.Balance > 0m do
                                                                                        let acct, stack = payback (curAcct, curStack, [])
                                                                                        curAcct <- acct
                                                                                        curStack <- stack |> List.filter (fun x -> x.PaidDate.IsNone) |> List.rev
                                                                                        curResults <- stack |> List.filter (fun x -> x.PaidDate.IsSome) |> List.append curResults
                                                                                    screen (accts, curStack, curAcct :: curResults)
                       | [], stack, results                        -> stack, results
        screen (accts, [], [])

