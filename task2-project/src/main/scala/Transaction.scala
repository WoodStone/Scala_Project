import java.util.concurrent.ConcurrentLinkedQueue

import exceptions._

import scala.collection.convert.WrapAsScala

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {

  val queue: ConcurrentLinkedQueue[Transaction] = new ConcurrentLinkedQueue[Transaction]()

  // Remove and return the first element from the queue
  def pop: Transaction = queue poll()

  // Return whether the queue is empty
  def isEmpty: Boolean = queue.isEmpty

  // Add new element to the back of the queue
  def push(t: Transaction): Unit = queue add t

  // Return the first element from the queue without removing it
  def peek: Transaction = queue peek()

  // Return an iterator to allow you to iterate over the queue
  def iterator: Iterator[Transaction] = WrapAsScala.asScalaIterator(queue iterator())
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttemps: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING

  override def run: Unit = {

    def doTransaction() = {
      from withdraw amount
      to deposit amount
    }

    try {
      if (allowedAttemps < 1) {
        throw new IllegalAmountException()
      }
      if (from.uid < to.uid) from synchronized {
        to synchronized {
          doTransaction
        }
      } else to synchronized {
        from synchronized {
          doTransaction
        }
      }
      status = TransactionStatus.SUCCESS
    } catch {
      case iae: IllegalAmountException => status = TransactionStatus.FAILED
      case noFunds: NoSufficientFundsException =>
        transactionsQueue.push(new Transaction(transactionsQueue, processedTransactions, from, to, amount, allowedAttemps - 1))
    } finally {
      if (status != TransactionStatus.PENDING) {
        processedTransactions.push(this)
      }
    }
    // Extend this method to satisfy new requirements.

  }
}
