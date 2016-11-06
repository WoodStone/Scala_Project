import exceptions.{IllegalAmountException, NoSufficientFundsException}

class Account(var initialBalance: Double, val uid: Int = Bank getUniqueId) {
  def withdraw(amount: Double): Unit = {
    if (amount < 0) {
      throw new IllegalAmountException()
    }
    if (initialBalance - amount < 0) {
      throw new NoSufficientFundsException()
    }
    this.synchronized {
      initialBalance -= amount
    }
  }
  def deposit(amount: Double): Unit = {
    if (amount < 0) {
      throw new IllegalAmountException()
    }
    this.synchronized {
      initialBalance += amount
    }
  }
  def getBalanceAmount: Double = {
    initialBalance
  }
}
