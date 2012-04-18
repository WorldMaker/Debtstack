namespace Debtstack

module Strategies =
    let reset (txs : seq<TransactionState>) = txs |> Seq.iter (fun tx -> tx.Reset ())

    let simple (txs : list<TransactionState>) =
        reset txs
        let pay (tx : TransactionState) credit = let amt = min -tx.Remaining credit
                                                 match tx.Contract.Transaction.Type with
                                                 | Debit | Interest when tx.Remaining < 0m -> tx.Contract.Paid <- tx.Contract.Paid + amt
                                                                                              credit - amt
                                                 | Credit                                  -> credit + tx.Contract.Transaction.Value
                                                 | _                                       -> credit
        let rec s1 = function
                     | tx :: txs, credit -> s1 (txs, pay tx credit)
                     | [], _             -> ()
        s1 (txs |> List.rev, 0m)

    let simpleStack (txs : list<TransactionState>) =
        reset txs
        let pay (tx : TransactionState) credit = let amt = min -tx.Remaining credit
                                                 match tx.Contract.Transaction.Type with
                                                 | Debit | Interest when tx.Remaining < 0m -> tx.Contract.Paid <- tx.Contract.Paid + amt
                                                                                              credit - amt
                                                 | _                                       -> credit
        let rec s1 = function
                     | tx :: txs, credit -> s1 (txs, pay tx credit)
                     | [], _             -> ()
        let rec s2 = function
                     | (tx : TransactionState) :: txs, stack -> match tx.Contract.Transaction.Type with
                                                                | Debit | Interest -> s2 (txs, tx :: stack)
                                                                | Credit           -> s1 (stack, tx.Contract.Transaction.Value)
                                                                                      s2 (txs, stack |> List.filter (fun x -> x.Remaining < 0m))
                     | [], stack                             -> ()
        s2 (txs, [])

    let favorTheOld (txs : list<TransactionState>) =
        reset txs
        let pay (tx : TransactionState) credit = let amt = min -tx.Remaining credit
                                                 match tx.Contract.Transaction.Type with
                                                 | Debit | Interest when tx.Remaining < 0m -> tx.Contract.Paid <- tx.Contract.Paid + amt
                                                                                              amt
                                                 | _                                       -> 0m
        let rec s1 = function
                     | tx :: txs, credit -> s1 (txs, credit - (pay tx (credit / (decimal txs.Length + 1m))))
                     | [], _             -> ()
        let rec s2 = function
                     | (tx : TransactionState) :: txs, stack -> match tx.Contract.Transaction.Type with
                                                                | Debit | Interest -> s2 (txs, tx :: stack)
                                                                | Credit           -> s1 (stack, tx.Contract.Transaction.Value)
                                                                                      s2 (txs, stack |> List.filter (fun x -> x.Remaining < 0m))
                     | [], stack                             -> ()
        s2 (txs, [])

