import scala.io.StdIn.readLine
import scala.util.Random

case class Transactions(transactionId: Long, accountNumber: Long, transactionType: String, amount: Double)

class Bank {
  val accounts: Map[Long, Double] = Map()

  //Method is used to create the new Account
  def createAccount(openingBalance: Double): Map[Long, Double] = {
    val accountNumber = Random.between(minInclusive = 231289128912L, maxExclusive = 988756987654L)
    val accounts = Map(accountNumber -> openingBalance)
    accounts
  }

  // This method is used for creating multiple account and inside it we call the above method
  def newAccountsCreation(numberOfAccounts: Int, acc: Map[Long, Double]): Map[Long, Double] = {
    val openingBalance = readLine("Enter the Amount of opening Balance greater than 1000 :").toDouble
    try {
      if (openingBalance.isNaN) {
        throw new NumberFormatException("Invalid Number")
      }
      else if (openingBalance < 1000 || openingBalance <= 0) {
        throw new ArithmeticException("Minimum amount should be 1000")
      }
      else {
        if (numberOfAccounts <= 1) acc ++ createAccount(openingBalance)
        else newAccountsCreation(numberOfAccounts - 1, acc ++ createAccount(openingBalance))
      }
    }
    catch {
      case ex: NumberFormatException => throw new NumberFormatException("Failed to Change the position to String")
      case ex: ArithmeticException => throw new ArithmeticException("0 Balance is not acceptable")
    }
  }

  // Method used to list the Accounts
  def listAccounts(accountDetails: Map[Long, Double]): Map[Long, Double] = {
    try {
      if (accountDetails.isEmpty) throw new NullPointerException("No Account Exists in the List")
      else {
        val accountList = accountDetails
        accountList
      }
    }
    catch {
      case ex: NullPointerException => throw new NullPointerException("No Account Exists in the List")
    }
  }

  //Method used to fetch the account balance as per the account number
  def fetchAccountBalance(accountNumber: Long, accountDetail: Map[Long, Double]): Double = {
    if (accountDetail.isEmpty) 0.0
    else if (accountDetail.contains(accountNumber)) {
      val balance = accountDetail(accountNumber)
      val balanceFormat = f"$balance%.4f"
      balanceFormat.toDouble
    }
    else 0.0
  }


  //This method is used to delete the account
  def deleteAccount(accountNumber: Long, accountDetails: Map[Long, Double]): Boolean = {
    if (accountDetails.contains(accountNumber)) {
      val updatedAccount = accountDetails.removed(accountNumber)
      println("Account After Deleted\n"+updatedAccount)
      true
    }
    else false
  }

  //This method is used to update the balance after the transactions
  def updateBalance(transactions: List[Transactions], accountDetail: Map[Long, Double]): Map[Long, Double] = {
    val accountBalance = transactions.map(transaction => {
      val balance = accountDetail.getOrElse(transaction.accountNumber, 0.0)
      val updateBalance = transaction.transactionType.toLowerCase match {
        case "credit" => balance + transaction.amount
        case "debit" => balance - transaction.amount
        case _ => balance
      }
      accountDetail.updated(transaction.accountNumber, updateBalance)
    })
    accountBalance.flatMap(_.toList).toMap
  }
}

//Main Start from here(Singleton Object)
object BankingApplication extends App {
  val bankApp = new Bank
  val numberOfAccounts: Int = readLine("Enter the no. of account you have to create  :").toInt
  val accounts: Map[Long, Double] = Map()

  //  To Create multiple  Account
  val accountDetails = bankApp.newAccountsCreation(numberOfAccounts, accounts)
  println("\n1.After Creating Account:\n" + accountDetails)

  //To view the AccountList
  val accountsList = bankApp.listAccounts(accountDetails)
  println("\n2.List of All the Accounts are :     \n" + accountsList)

  //To access the Amount of particular Customer
  val accountNumberList = accountDetails.keys.toList
  println("\n3.Enter 1,2 as per order to fetch the account balance from given List \n" + accountNumberList)

  val fetchAccount = readLine("Select the above order to show account Balance from 1 to shown list number   :").toInt
  val accountBalance: Double = bankApp.fetchAccountBalance(accountNumberList(fetchAccount - 1), accountDetails)
  println("Your account balance is   :" + accountBalance)

  //Transactions Operation
  val transactions = List(
    Transactions(3l, accountNumberList(0), "Credit", 12000.0),
    Transactions(3l, accountNumberList(0), "Debit", 1000.0)
  )
  val balanceAfterTransactions = bankApp.updateBalance(transactions, accountDetails)
  println("\n4.List of the Account After Updating balance :\n" + balanceAfterTransactions)

  //  To delete the Account
  val orderOfAccountToDelete = readLine("\nSelect the above sequence order to delete account Balance : ").toInt
  try {
    if ((orderOfAccountToDelete) < 0) throw new ArrayIndexOutOfBoundsException()
    else {
      val deleteAccount = bankApp.deleteAccount(accountNumberList(orderOfAccountToDelete - 1), accountDetails)
      println("Account found & Deleted Account :::" + deleteAccount)
    }
  }
  catch {
    case ex: ArrayIndexOutOfBoundsException => println("Selected Wrong Input ")
  }


}
