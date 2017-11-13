package us.zuercher.divvy

import org.scalatest.{FunSpec, Matchers}

class SolverTest extends FunSpec with Matchers {
  describe("Solver") {
    it("should assign all payments to a single creditor") {
      Solver(
        Seq(Debtor("a", Amount(50)), Debtor("b", Amount(50))),
        Seq(Creditor("c", Amount(-100)))
      ) should contain theSameElementsAs(Seq(
        Payment(Relationship("a", "c"), Amount(50)),
        Payment(Relationship("b", "c"), Amount(50))
      ))
    }

    it("should assign all payments to a single debtor") {
      Solver(
        Seq(Debtor("a", Amount(50))),
        Seq(Creditor("c", Amount(-50)))
      ) should contain theSameElementsAs(Seq(
        Payment(Relationship("a", "c"), Amount(50))
      ))
    }

    it("should assign all payments to a single debtor from multiple creditors") {
      Solver(
        Seq(Debtor("a", Amount(100))),
        Seq(Creditor("c", Amount(-50)), Creditor("d", Amount(-50)))
      ) should contain theSameElementsAs(Seq(
        Payment(Relationship("a", "c"), Amount(50)),
        Payment(Relationship("a", "d"), Amount(50))
      ))
    }

    it("should optimize for fewer, larger payments") {
      Solver(
        Seq(Debtor("a", Amount(100)), Debtor("b", Amount(10)), Debtor("c", Amount(10))),
        Seq(Creditor("x", Amount(-100)), Creditor("y", Amount(-20)))
      ) should contain theSameElementsAs(Seq(
        Payment(Relationship("a", "x"), Amount(100)),
        Payment(Relationship("b", "y"), Amount(10)),
        Payment(Relationship("c", "y"), Amount(10))
      ))
    }
  }
}
