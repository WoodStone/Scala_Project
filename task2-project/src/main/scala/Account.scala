import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

  class Balance(var amount: Double) {}

  val balance = new Balance(initialBalance)
  val uid = bank.generateAccountId

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

  def transferTo(account: Account, amount: Double) = {
    bank addTransactionToQueue (this, account, amount)
  }


}
