// Copyright 2012 Max Battcher. Some rights reserved.
// Licensed for use under the Ms-RL. See attached LICENSE file.
namespace Debtstack

open ImpromptuInterface.FSharp

module Strategies =
    exception MoneyProblems of string

    let roundpenny money = truncate (money * 100m) / 100m

    let reset (txs : seq<TransactionState>) = txs |> Seq.iter (fun tx -> tx.Reset ())

    let simpleStack (txs : list<TransactionState>) =
        reset txs
        let rec payback = function
                        | (tx : TransactionState) :: txs, credit, date -> payback (txs, credit - tx.Pay credit date, date)
                        | [], credit, _ when credit > 0m               -> raise (MoneyProblems "Payback failed")
                        | [], _, _                                     -> ()
        let rec interesting = function
                            | (tx : TransactionState) :: txs, interest -> tx.Interest interest
                                                                          interest + interesting (txs, interest)
                            | [], _                                    -> 0m
        let rec screen = function
                       | (tx : TransactionState) :: txs, stack -> match tx.Proxy.Transaction.Type with
                                                                  | Debit    -> screen (txs, tx :: stack)
                                                                  | Interest -> let interest = tx.Proxy.Transaction.Value
                                                                                let portion = roundpenny (interest / decimal stack.Length)
                                                                                stack.Head.Interest (interest - interesting (stack.Tail, portion))
                                                                                screen (txs, stack)
                                                                  | Credit   -> payback (stack, tx.Proxy.Transaction.Value, tx.Proxy.Transaction.Date)
                                                                                screen (txs, stack |> List.filter (fun x -> x.Proxy.PaidDate.IsNone))
                       | [], stack                             -> ()
        screen (txs, [])

    let favorTheOld (txs : list<TransactionState>) =
        reset txs
        let rec payback = function
                        | (tx : TransactionState) :: txs, credit, date -> payback (txs, credit - (tx.Pay (roundpenny (credit / (decimal txs.Length + 1m))) date), date)
                        | [], credit, _                                -> credit
        let rec interesting = function
                            | (tx : TransactionState) :: txs, interest, sum -> let intamt = roundpenny (interest * (tx.TrueRemaining / sum))
                                                                               tx.Interest intamt
                                                                               intamt + interesting (txs, interest, sum)
                            | [], interest, _                               -> 0m
        let rec screen = function
                       | (tx : TransactionState) :: txs, stack -> match tx.Proxy.Transaction.Type with
                                                                  | Debit    -> screen (txs, tx :: stack)
                                                                  | Interest -> let interest = tx.Proxy.Transaction.Value
                                                                                stack.Head.Interest (interest - interesting (stack.Tail, interest, stack |> List.sumBy (fun tx -> tx.TrueRemaining)))
                                                                                screen (txs, stack)
                                                                  | Credit   -> let mutable credit = tx.Proxy.Transaction.Value
                                                                                while credit > 0m do
                                                                                    credit <- payback (stack |> List.filter (fun x -> x.Proxy.PaidDate.IsNone), credit, tx.Proxy.Transaction.Date)
                                                                                screen (txs, stack |> List.filter (fun x -> x.Proxy.PaidDate.IsNone))
                       | [], stack                             -> ()
        screen (txs, [])

