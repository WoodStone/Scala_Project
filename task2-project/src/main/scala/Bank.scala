import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.ExecutionContext
import scala.concurrent.forkjoin.ForkJoinPool

class Bank(val allowedAttempts: Integer = 3) {

  private val uid = new AtomicInteger()
  private val transactionsQueue: TransactionQueue = new TransactionQueue()
  private val processedTransactions: TransactionQueue = new TransactionQueue()
  private val executorContext = ExecutionContext.fromExecutorService(new ForkJoinPool(12))
  executorContext.execute(new Runnable {
    override def run(): Unit = {
      try {
        while (!Thread.interrupted()) {
          processTransactions
          Thread.sleep(10)
        }
      } finally {

      }
    }
  })

  def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
    transactionsQueue push new Transaction(
      transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
  }

  def generateAccountId: Int = {
    val current = uid get()
    val updated = current + 1
    if (uid.compareAndSet(current, updated)) {
      return updated
    }
    generateAccountId
  }

  private def processTransactions: Unit = {
    while (!transactionsQueue.isEmpty) {
      executorContext.execute(transactionsQueue.pop)
    }
  }

  def addAccount(initialBalance: Double): Account = {
    new Account(this, initialBalance)
  }

  def getProcessedTransactionsAsList: List[Transaction] = {
    processedTransactions.iterator.toList
  }

}
