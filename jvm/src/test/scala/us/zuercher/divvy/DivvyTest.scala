package us.zuercher.divvy

import org.scalatest.{FunSpec, Matchers}

class DivvyTest extends FunSpec with Matchers {
  describe("Divvy") {
    it("should ignore expenses where the single debtor is the creditor") {
      Divvy(
        Seq(Spend("a", Amount(100), "x", Seq("a"))),
        false
      ) should be(empty)
    }

    it("should ignore expenses with no debtors") {
      Divvy(
        Seq(Spend("a", Amount(100), "x", Seq.empty)),
        false
      ) should be(empty)
    }

    describe("when all creditors are also debtors") {
      it("should handle a single payment") {
        val all = Seq("a", "b", "c")
        Divvy(
          Seq(
            Spend("a", Amount(66), "x", all),
            Spend("b", Amount(33), "y", all)
          ),
          false
        ) should contain theSameElementsAs(Seq(
          Payment(Relationship("c", "a"), Amount(33))
        ))
      }

      it("should handle multiple payments to a creditor") {
        val all = Seq("a", "b", "c", "d", "e")
        Divvy(
          Seq(
            Spend("a", Amount(10000), "x", all),
            Spend("b", Amount(1250), "y", all),
            Spend("c", Amount(1250), "z", all)
          ),
          false
        ) should contain theSameElementsAs(Seq(
          Payment(Relationship("b", "a"), Amount(1250)),
          Payment(Relationship("c", "a"), Amount(1250)),
          Payment(Relationship("d", "a"), Amount(2500)),
          Payment(Relationship("e", "a"), Amount(2500))
        ))
      }

      it("should handle multiple payments to multiple creditors") {
        val all = Seq("a", "b", "c", "d", "e")
        Divvy(
          Seq(
            Spend("a", Amount(5000), "x", all),
            Spend("b", Amount(5000), "y", all),
            Spend("c", Amount(2500), "z", all)
          ),
          false
        ) should contain theSameElementsAs(Seq(
          Payment(Relationship("e", "a"), Amount(2500)),
          Payment(Relationship("d", "b"), Amount(2500))
        ))
      }

      it("should handle expenses not paid by all debtors") {
        val all = Seq("a", "b", "c", "d", "e")
        val diners = Seq("a", "b", "c")

        Divvy(
          Seq(
            Spend("a", Amount(9000), "dinner", diners),
            Spend("b", Amount(5000), "x", all),
            Spend("c", Amount(2500), "y", all)
          ),
          false
        ) should contain theSameElementsAs(Seq(
          Payment(Relationship("c", "a"), Amount(1500)),
          Payment(Relationship("d", "a"), Amount(1500)),
          Payment(Relationship("e", "a"), Amount(1500)),
          Payment(Relationship("c", "b"), Amount(500))
        ))
      }
    }

    describe("when some creditors are not debtors") {
      it("should fully reimburse non-participants") {
        val all = Seq("a", "b", "c", "d")
        Divvy(
          Seq(
            Spend("x", Amount(100), "x", all),
            Spend("a", Amount(100), "y", all)
          ),
          false
        ) should contain theSameElementsAs(Seq(
          Payment(Relationship("d", "a"), Amount(50)),
          Payment(Relationship("b", "x"), Amount(50)),
          Payment(Relationship("c", "x"), Amount(50))
        ))
      }
    }
  }
}
