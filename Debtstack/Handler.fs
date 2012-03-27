namespace Debtstack

[<Interface>]
type IDebtstack =
    abstract member Transactions : seq<TransactionState> with get, set

