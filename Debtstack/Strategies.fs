﻿// Copyright 2012 Max Battcher. Some rights reserved.
// Licensed for use under the Ms-RL. See attached LICENSE file.
namespace Debtstack

open ImpromptuInterface.FSharp

module Strategies =
    exception MoneyProblems of string

    let roundpenny money = truncate (money * 100m) / 100m

    let adjust acct tx = { acct with Transactions = tx :: acct.Transactions }

    let pay = function
        | from, (acct : Account), amt when acct.Interest < 0m && abs acct.Interest < amt -> let txs = { Type = TransactionType.Adjustment; Name = from.Name; Amount = amt + acct.Interest; Date = from.Date; Key = 0 }
                                                                                                    :: { Type = TransactionType.InterestPaid; Name = from.Name; Amount = -acct.Interest; Date = from.Date; Key = 0 }
                                                                                                    :: acct.Transactions
                                                                                            { acct with Transactions = txs }
        | from, (acct : Account), amt when acct.Interest < 0m -> adjust acct { Type = TransactionType.InterestPaid; Name = from.Name; Amount = amt; Date = from.Date; Key = 0 }
        | from, (acct : Account), amt -> adjust acct { Type = TransactionType.Adjustment; Name = from.Name; Amount = amt; Date = from.Date; Key = 0 }

    let simpleStack (accts : list<Account>) =
        let rec payback = function
                        | (from : Account), (next : Account) :: more, results 
                          when from.Balance > 0m && next.Balance < 0m -> let bal = abs next.Balance
                                                                         let amt = min from.Balance bal
                                                                         payback (adjust from { Type = TransactionType.Adjustment; Name = next.Name; Amount = -amt; Date = from.Date; Key = 0 }
                                                                            , more
                                                                            , pay (from, next, amt) :: results)
                        | (from : Account), (next : Account) :: more, results -> payback (from, more, next :: results)
                        | from, [], results when from.Balance > 0m -> raise (MoneyProblems "Payback failed")
                        | from, [], results -> from, results

    
        let rec interesting = function
                | from, amt, [next], results -> (adjust from { Type = TransactionType.Adjustment; Name = next.Name; Amount = -from.Balance; Date = from.Date; Key = 0 }
                                                , adjust next { Type = TransactionType.Interest; Name = from.Name; Amount = from.Balance; Date = from.Date; Key = 0 } :: results)
                | (from : Account), amt, (next : Account) :: more, results 
                    when from.Balance < 0m -> interesting (adjust from { Type = TransactionType.Adjustment; Name = next.Name; Amount = -amt; Date = from.Date; Key = 0 }
                                                            , amt
                                                            , more
                                                            , adjust next { Type = TransactionType.Interest; Name = from.Name; Amount = amt; Date = from.Date; Key = 0 } :: results)
                | (from : Account), amt, (next : Account) :: more, results -> interesting (from, amt, more, next :: results)
                | from, _, [], results when from.Balance < 0m -> raise (MoneyProblems "Interest failed")
                | from, _, [], results -> from, results

        let rec screen = function
                       | (acct : Account) :: accts, stack, results -> match acct.Type with
                                                                      | AccountType.Debit    -> screen (accts, acct :: stack, results)
                                                                      | AccountType.Interest -> let amt = roundpenny (acct.Balance / (decimal stack.Length))
                                                                                                let acct, stack = interesting (acct, amt, stack, [])
                                                                                                screen (accts, List.rev stack, acct :: results)
                                                                      | AccountType.Credit   -> let acct, stack = payback (acct, stack, [])
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
                                                                         payback (adjust from { Type = TransactionType.Adjustment; Name = next.Name; Amount = -amt; Date = from.Date; Key = 0 }
                                                                            , more
                                                                            , pay (from, next, amt) :: results)
                        | (from : Account), (next : Account) :: more, results -> payback (from, more, next :: results)
                        // | from, [], results when from.Balance > 0m -> raise (MoneyProblems "Payback failed")
                        | from, [], results -> from, results

                            
        let rec interesting = function
                | from, amt, [next], results -> (adjust from { Type = TransactionType.Adjustment; Name = next.Name; Amount = -from.Balance; Date = from.Date; Key = 0 }
                                                , adjust next { Type = TransactionType.Interest; Name = from.Name; Amount = from.Balance; Date = from.Date; Key = 0 } :: results)
                | (from : Account), amt, (next : Account) :: more, results 
                    when from.Balance < 0m -> let part = roundpenny (amt * next.Balance)
                                              interesting (adjust from { Type = TransactionType.Adjustment; Name = next.Name; Amount = -part; Date = from.Date; Key = 0 }
                                                            , amt
                                                            , more
                                                            , adjust next { Type = TransactionType.Interest; Name = from.Name; Amount = part; Date = from.Date; Key = 0 } :: results)
                | (from : Account), amt, (next : Account) :: more, results -> interesting (from, amt, more, next :: results)
                | from, _, [], results when from.Balance < 0m -> raise (MoneyProblems "Interest failed")
                | from, _, [], results -> from, results

        let rec screen = function
                       | (acct : Account) :: accts, stack, results -> match acct.Type with
                                                                      | AccountType.Debit    -> screen (accts, acct :: stack, results)
                                                                      | AccountType.Interest -> let amt = acct.Balance / (stack |> Seq.sumBy (fun x -> x.Balance))
                                                                                                let acct, stack = interesting (acct, amt, stack, [])
                                                                                                screen (accts, List.rev stack, acct :: results)
                                                                      | AccountType.Credit   -> let mutable curAcct, curStack, curResults = acct, stack, results
                                                                                                while curAcct.Balance > 0m do
                                                                                                    let acct, stack = payback (curAcct, curStack, [])
                                                                                                    curAcct <- acct
                                                                                                    curStack <- stack |> List.filter (fun x -> x.PaidDate.IsNone) |> List.rev
                                                                                                    curResults <- stack |> List.filter (fun x -> x.PaidDate.IsSome) |> List.append curResults
                                                                                                screen (accts, curStack, curAcct :: curResults)
                       | [], stack, results                        -> stack, results
        screen (accts, [], [])

    let naive (accts : list<Account>) =
        let credits = accts |> Seq.filter (fun x -> x.Type = AccountType.Credit)
        let debits = accts |> Seq.filter (fun x -> x.Type <> AccountType.Credit) |> Seq.toArray
        let mutable results: list<Account> = []
        for credit in credits do
            let mutable c = credit
            for i in 0 .. debits.Length - 1 do
                let d = debits.[i]
                if c.Balance > 0m && d.Balance < 0m then let amt = min (abs d.Balance) c.Balance
                                                         c <- adjust c { Type = TransactionType.Adjustment; Name = d.Name; Amount = -amt; Date = c.Date; Key = 0 }
                                                         debits.[i] <- adjust d { Type = TransactionType.Adjustment; Name = c.Name; Amount = amt; Date = c.Date; Key = 0 }

            if c.Balance > 0m then raise (MoneyProblems "Payment failed")
            results <- c :: results
        results, debits