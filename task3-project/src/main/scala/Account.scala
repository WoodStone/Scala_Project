import akka.actor._
import exceptions._
import scala.collection.immutable.HashMap

case class TransactionRequest(toAccountNumber: String, amount: Double)

case class TransactionRequestReceipt(toAccountNumber: String,
                                     transactionId: String,
                                     transaction: Transaction)

case class BalanceRequest()

class Account(val accountId: String, val bankId: String, val initialBalance: Double = 0) extends Actor {

  private var transactions = HashMap[String, Transaction]()

  class Balance(var amount: Double) {}

  val balance = new Balance(initialBalance)

  def getFullAddress: String = {
    bankId + accountId
  }

  def getTransactions: List[Transaction] = {
    // Should return a list of all Transaction-objects stored in transactions
    transactions.values.toList
  }

  def allTransactionsCompleted: Boolean = {
    // Should return whether all Transaction-objects in transactions are completed
    for (trans <- getTransactions) {
      if (!trans.isCompleted) {
        return false
      }
    }
    return true
  }

  def withdraw(amount: Double): Unit = {
    if (amount < 0) {
      throw new IllegalAmountException()
    }
    if (balance.amount - amount < 0) {
      throw new NoSufficientFundsException()
    }
    this.synchronized {
      balance.amount -= amount
    }
  }
  def deposit(amount: Double): Unit = {
    if (amount < 0) {
      throw new IllegalAmountException()
    }
    this.synchronized {
      balance.amount += amount
    }
  }
  def getBalanceAmount: Double = {
    balance.amount
  }

  def sendTransactionToBank(t: Transaction): Unit = {
    // Should send a message containing t to the bank of this account
    BankManager.findBank(bankId) ! t
  }

  def transferTo(accountNumber: String, amount: Double): Transaction = {

    val t = new Transaction(from = getFullAddress, to = accountNumber, amount = amount)

    if (reserveTransaction(t)) {
      try {
        withdraw(amount)
        sendTransactionToBank(t)

      } catch {
        case _: NoSufficientFundsException | _: IllegalAmountException =>
          t.status = TransactionStatus.FAILED
      }
    }

    t

  }

  def reserveTransaction(t: Transaction): Boolean = {
    if (!transactions.contains(t.id)) {
      transactions += (t.id -> t)
      return true
    }
    false
  }

  override def receive = {
    case IdentifyActor => sender ! this

    case TransactionRequestReceipt(to, transactionId, transaction) => {
      // Process receipt
      if (transaction.isSuccessful) {
        transactions += (transactionId -> transaction)
      }
    }

    case BalanceRequest => sender ! getBalanceAmount // Should return current balance

    case t: Transaction => {
      // Handle incoming transaction
      deposit(t.amount)
      t.status = TransactionStatus.SUCCESS
      BankManager.findAccount(t.from.substring(0, 4), t.from.substring(4)) ! TransactionRequestReceipt(getFullAddress, t.id, t)
    }

    case msg => ???
  }


}
