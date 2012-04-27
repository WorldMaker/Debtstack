namespace Debtstack

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
                                                                          interesting (txs, interest)
                            | [], _                                    -> ()
        let rec screen = function
                       | (tx : TransactionState) :: txs, stack -> match tx.Contract.Transaction.Type with
                                                                  | Debit    -> screen (txs, tx :: stack)
                                                                  | Interest -> interesting (stack, tx.Contract.Transaction.Value / decimal stack.Length)
                                                                                screen (txs, stack)
                                                                  | Credit   -> payback (stack, tx.Contract.Transaction.Value, tx.Contract.Transaction.Date)
                                                                                screen (txs, stack |> List.filter (fun x -> x.Contract.PaidDate.IsNone))
                       | [], stack                             -> ()
        screen (txs, [])

    let favorTheOld (txs : list<TransactionState>) =
        reset txs
        let rec payback = function
                        | (tx : TransactionState) :: txs, credit, date -> payback (txs, credit - (tx.Pay (roundpenny (credit / (decimal txs.Length + 1m))) date), date)
                        | [], credit, _                                -> credit
        let rec interesting = function
                            | (tx : TransactionState) :: txs, interest, sum -> tx.Interest (interest * tx.Remaining / sum)
                                                                               interesting (txs, interest, sum)
                            | [], _, _                                      -> ()
        let rec screen = function
                       | (tx : TransactionState) :: txs, stack -> match tx.Contract.Transaction.Type with
                                                                  | Debit    -> screen (txs, tx :: stack)
                                                                  | Interest -> interesting (stack, tx.Contract.Transaction.Value, stack |> List.sumBy (fun tx -> tx.Remaining))
                                                                                screen (txs, stack)
                                                                  | Credit   -> let mutable credit = tx.Contract.Transaction.Value
                                                                                while credit > 0m do
                                                                                    credit <- payback (stack |> List.filter (fun x -> x.Contract.PaidDate.IsNone), credit, tx.Contract.Transaction.Date)
                                                                                screen (txs, stack |> List.filter (fun x -> x.Contract.PaidDate.IsNone))
                       | [], stack                             -> ()
        screen (txs, [])

