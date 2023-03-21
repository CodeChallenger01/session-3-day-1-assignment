import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class BankApplicationTest extends AnyFlatSpec with Matchers {
  val accounts: Map[Long, Double] = Map()
  val bankApp = new Bank
  val accountDetails: Map[Long, Double] = accounts ++ bankApp.createAccount(15000) ++ bankApp.createAccount(14000)
  val listOfAccountNumber: List[Long] = accountDetails.keys.toList
  val listOfAccounts: List[Double] = accountDetails.values.toList

  //To check with empty account
  "Empty Account" should "match when we check with empty account" in {
    val isAccountEmpty = accounts.isEmpty
    assert(isAccountEmpty == true)
  }

  //To check the amount of first account
  "it " should "match with the amount of first account " in {
    val openingBalance = 12000.0
    val account = bankApp.createAccount(openingBalance)
    val accountAmount = account.values.toList
    accountAmount(0) shouldBe openingBalance
  }

  //to check the size of existing account
  "it" should " match with exist account size and including inside account as well" in {
    val newAccount = accountDetails ++ bankApp.createAccount(20000)
    val noOfAccounts = newAccount.size
    assert(noOfAccounts == newAccount.size)
  }

  //It check with account list
  "listAccount" should "return the list of accounts" in {
    val listOfAccounts = bankApp.listAccounts(accountDetails)
    val actualOutput = accountDetails
    listOfAccounts shouldBe actualOutput
  }

  //To check with account list manually by passing all account parameter
  "it " should "contains all the account" in {
    val listOfAccountNumber = accountDetails.keys.toList
    val listOfAmounts = accountDetails.values.toList
    bankApp.listAccounts(accountDetails) shouldBe Map(listOfAccountNumber(0) -> listOfAmounts(0), listOfAccountNumber(1) -> listOfAmounts(1))
  }

  //It will check that list of account isEmpty
  "listAccount" should "not return empty map because it contains the account" in {
    val listOfAccount = bankApp.listAccounts(accountDetails)
    assert(listOfAccount != null)
  }

  //To fetch the account balance by passing the account number as parameter
  "fetch Account Balance" should " return the bank balance" in {
    val accountNumber = accountDetails.keys.toList
    val fetchBalance = bankApp.fetchAccountBalance(accountNumber(0), accountDetails)
    val expectedBalanceOfFirstAccount = accountDetails.values.toList(0)
    fetchBalance shouldBe expectedBalanceOfFirstAccount
  }

  //check with other bank account balance as negative test case
  "fetch Account Balance" should " return the bank balance not equal to if we are checking with other" in {
    val accountNumber = accountDetails.keys.toList
    val fetchBalance = bankApp.fetchAccountBalance(accountNumber(0), accountDetails)
    val expectedBalanceOfFirstAccount = accountDetails.values.toList(1)
    assert(fetchBalance != expectedBalanceOfFirstAccount)
  }

  //delete account give true because it contains the account
  "delete Account" should "return true if account found and deleted" in {
    val accountNumber = accountDetails.keys.toList
    val isAccountDeleted = bankApp.deleteAccount(accountNumber(0), accountDetails)
    val expectedOutput = true
    isAccountDeleted shouldBe expectedOutput
  }

  //delete account give false because account number doesn't exist
  "delete Account" should "return false if account is not found" in {
    val accountNumber = 200202022002L
    val isAccountDeleted = bankApp.deleteAccount(accountNumber, accountDetails)
    val expectedOutput = true
    assert(isAccountDeleted != true)
  }

  //It should match with updated balance after credit and debit
  "update Balance" should " match with updated balance" in {
    val accountNumberList = accountDetails.keys.toList
    val transactions = List(
      Transactions(3l, accountNumberList(0), "Credit", 12000.0)
    )
    val balance = bankApp.updateBalance(transactions, accountDetails).values.toList(0)
    val expectedBalance = accountDetails.values.toList(0) + 12000.0
    balance shouldBe expectedBalance
  }

  //To check with negative test case after giving another  account number
  "updated Balance" should "not match with balance if account number not exist" in {
    val accountNumber = 200202022002L
    val transactions = List(
      Transactions(3l, accountNumber, "Credit", 12000.0)
    )
    val balance = bankApp.updateBalance(transactions, accountDetails).values.toList(0)
    println(balance)
    val expectedBalance = 0
    assert(balance != expectedBalance)
  }

}
