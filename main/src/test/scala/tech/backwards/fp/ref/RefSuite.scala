package tech.backwards.fp.ref

import cats.data.NonEmptyList
import weaver._
import cats.effect._
import cats.implicits._
import monocle.Lens
import monocle.macros.GenLens

/**
 * [[https://www.pluralsight.com/tech-blog/scala-cats-effect-ref/ Cats Effect Ref]]
 *
 * Ref is a mutable reference which:
 *  - Is non-blocking
 *  - Is accessed and modified concurrently
 *  - Is modified atomically
 *  - Is dealt with using only pure functions
 */
object RefSuite extends SimpleIOSuite {
  /**
   * {{{
   *  def of[F[_]: Sync, A](a: A): F[Ref[F, A]] =
   *    Synch[F].delay(new Ref[F, A](new AtomicReference[A](a)))
   * }}}
   */
  test("Ref.of") {
    val ref: IO[Ref[IO, Int]] =
      Ref.of[IO, Int](42)

    ref.flatMap(_.get).map(x => expect(x == 42))
  }

  /**
   * {{{
   *  def get: F[A] =
   *    Sync[F].delay(ar.get) // Where "ar" is the AtomicReference supplied within the of constructor.
   * }}}
   */
  test("Ref.get - showing that Ref takes advantage of partially applied types (i.e. using Ref[IO].of instead of Ref.of[IO, Int])") {
    for {
      ref <- Ref[IO].of(42)
      contents <- ref.get
    } yield
      expect(contents == 42)
  }

  test("Ref.set - which relies on the same volatile and thread-safe nature of AtomicReference.") {
    for {
      ref <- Ref[IO].of(42)
      _ <- ref.set(21)
      contents <- ref.get
    } yield
      expect(contents == 21)
  }

  /**
   * {{{
   *  def update(f: A => A): F[Unit] =
   *    modify(a => (f(a), ()))
   * }}}
   */
  test("Ref.update - provides atomic version of a get then set") {
    // We DO NOT want to do the following:
    def getThenSet(ref: Ref[IO, Int]): IO[Unit] =
      ref.get.flatMap(contents =>
        ref.set(contents + 1)
      )

    val doNotDo: IO[Unit] =
      for {
        ref <- Ref[IO].of(42)
        _ <- NonEmptyList.of(getThenSet(ref), getThenSet(ref)).parSequence
        contents <- ref.get
      } yield
        println(contents) // Non-deterministic... could result in 43 or 44

    doNotDo *> (for {
      ref <- Ref[IO].of(42)
      _ <- ref.update(_ + 1)
      _ <- ref.update(_ + 1)
      contents <- ref.get
    } yield
      expect(contents == 44))
  }

  /**
   * {{{
   *  def modify[B](f: A => (A, B)): F[B] = {
   *    @tailrec
   *    def spin: B = {
   *      val c = ar.get
   *      val (u, b) = f(c)
   *
   *      if (!ar.compareAndSet(c, u)) spin
   *      else b
   *    }
   *
   *    Sync[F].delay(spin)
   *  }
   * }}}
   * Compare and set (CAS) takes two parameters:
   *  1) the value c that we expect to be contained in the AtomicReference and
   *  2) the value u to which we want to update the AtomicReference.
   *
   * CAS will update the AtomicReference to the value of u if and only if the value currently held within the AtomicReference is equal to the value of c.
   * If the value of c is not equal to the value currently held in the AtomicReference, then compareAndSet does not update the reference and returns false to indicate such.
   * The CAS operation is atomic because both the compare and the set functionality happen in a single machine instruction.
   *
   * Looking at [[Ref#modify]]
   * This method takes a function f as a parameter.
   * This function accepts the value that is currently stored in the Ref and returns a tuple containing:
   *  1) the value with which to update the Ref and
   *  2) the value to return from the modify method.
   */
  test("Ref.modify") {
    for {
      ref <- Ref[IO].of(42)
      outcome <- ref.modify(current => (current + 10, current + 100))
      newCurrent <- ref.get
    } yield
      expect(outcome == 142) and expect(newCurrent == 52)
  }

  test("Complete example - managing bank accounts") {
    type Balance = Int

    final case class BankAccount(number: String, balance: Balance)

    val balanceL: Lens[BankAccount, Balance] =
      GenLens[BankAccount](_.balance)

    final case class BankAccounts(ref: Ref[IO, Map[String, BankAccount]]) {
      def alterAmount(accountNumber: String, amount: Int): IO[Option[Balance]] =
        ref.modify { bankAccounts: Map[String, BankAccount] =>
          val bankAccount: Option[BankAccount] =
            bankAccounts.get(accountNumber).map(balanceL.modify(_ + amount))

          val updatedBankAccounts: Map[String, BankAccount] =
            bankAccounts ++ bankAccount.map(bankAccount => (bankAccount.number, bankAccount))

          val newBalance: Option[Balance] =
            bankAccount.map(_.balance)

          (updatedBankAccounts, newBalance)
        }

      def balance(accountNumber: String): IO[Option[Balance]] =
        ref.get.map(_.get(accountNumber).map(_.balance))

      def addAccount(account: BankAccount): IO[Unit] =
        ref.update(_ + (account.number -> account))
    }

    for {
      ref <- Ref[IO].of(Map.empty[String, BankAccount])
      bankAccounts = BankAccounts(ref)
      _ <- bankAccounts.addAccount(BankAccount("1", 0))
      _ <- bankAccounts.alterAmount("1", 50)
      nonExistingBalance <- bankAccounts.alterAmount("x", 100)
      endingBalance <- bankAccounts.alterAmount("1", -25)
      endBalance <- bankAccounts.balance("1")
    } yield
      expect(nonExistingBalance.isEmpty) and expect(endingBalance == 25.some) and expect(endBalance == 25.some)
  }
}