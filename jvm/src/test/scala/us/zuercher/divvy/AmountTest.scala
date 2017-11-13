package us.zuercher.divvy

import org.scalatest.{FunSpec, Matchers}

class AmountTest extends FunSpec with Matchers {
  describe("Amount") {
    describe("dollars & cents constructor") {
      it("should provide a contructor") {
        new Amount(1, 0) should equal(Amount(100))
      }

      it("should use absolute value of the cents") {
        new Amount(1, -1) should equal(Amount(101))
      }

      it("should handle negative dollars") {
        new Amount(-1, 1) should equal(Amount(-101))
      }
    }

    describe("addition") {
      it("should add two positive amounts") {
        Amount(100) + Amount(299) should equal(Amount(399))
      }

      it("should add two negative amounts") {
        Amount(-100) + Amount(-299) should equal(Amount(-399))
      }

      it("should add mixed sign amounts") {
        Amount(100) + Amount(-199) should equal(Amount(-99))
        Amount(100) + Amount(-99) should equal(Amount(1))
        Amount(-100) + Amount(199) should equal(Amount(99))
        Amount(-100) + Amount(99) should equal(Amount(-1))
      }
    }

    describe("subtraction") {
      it("should subtract positive amounts") {
        Amount(200) - Amount(99) should equal(Amount(101))
        Amount(200) - Amount(299) should equal(Amount(-99))
      }

      it("should subtract negative amounts") {
        Amount(-100) - Amount(-199) should equal(Amount(99))
        Amount(-100) - Amount(-99) should equal(Amount(-1))
      }

      it("should should subtract mixed sign amounts") {
        Amount(100) - Amount(-199) should equal(Amount(299))
        Amount(-100) - Amount(199) should equal(Amount(-299))
      }
    }

    describe("negation") {
      it("should negate a positive amount") {
        -Amount(99) should equal(Amount(-99))
      }

      it("should negate a negative amount") {
        -Amount(-99) should equal(Amount(99))
      }
    }

    describe("division") {
      it("should divide an amount evenly") {
        Amount(100) / 4 should equal(Amount(25))
        Amount(-100) / 4 should equal(Amount(-25))
      }

      it("should round when the amount cannot be divided evenly") {
        Amount(200) / 3 should equal(Amount(67))
        Amount(-200) / 3 should equal(Amount(-67))
      }
    }

    describe("multiplication") {
      it("should multiple a postive amount") {
        Amount(25) * 4 should equal(Amount(100))
      }

      it("should multiple a negative amount") {
        Amount(-25) * 4 should equal(Amount(-100))
      }
    }

    describe("split") {
      it("should produce several equal amounts for an even split") {
        Amount(1000).split(4) should equal(Seq.fill(4)(Amount(250)))
      }

      it("should should handle splitting nothing") {
        Amount.zero.split(2) should equal(Seq(Amount.zero, Amount.zero))
      }

      it("should handle splitting where some splits are zero") {
        Amount(1).split(4) should contain theSameElementsAs(
          Seq(Amount.zero, Amount.zero, Amount.zero, Amount.cent)
        )
      }

      it("should handle the case where division rounded down") {
        Amount(1000).split(3) should contain theSameElementsAs(
          Seq(Amount(333), Amount(333), Amount(334))
        )
      }

      it("should handle the case where division rounded up") {
        Amount(2000).split(3) should contain theSameElementsAs(
          Seq(Amount(667), Amount(667), Amount(666))
        )
      }

      it("should return different split orderings on successive calls") {
        Amount.reset()
        val first = Amount(100).split(6)
        val second = Amount(100).split(6)

        first should not equal(second)
      }

      it("should return the same split ordering across calls to reset") {
        Amount.reset()
        val first = Amount(100).split(3)
        Amount.reset()
        val second = Amount(100).split(3)

        first should equal(second)
      }
    }

    describe("comparisons") {
      it("should compare >") {
        Amount.cent > Amount.zero should equal(true)
        Amount(100) > Amount.zero should equal(true)
        Amount(100) > Amount(-100) should equal(true)
        Amount.zero > Amount.cent should equal(false)
        Amount.zero > Amount(100) should equal(false)
        Amount(-100) > Amount(100) should equal(false)
      }

      it("should compare <") {
        Amount.zero < Amount.cent should equal(true)
        Amount.zero < Amount(100) should equal(true)
        Amount(-100) < Amount(100) should equal(true)
        Amount.cent < Amount.zero should equal(false)
        Amount(100) < Amount.zero should equal(false)
        Amount(100) < Amount(-100) should equal(false)
      }
    }

    describe("toString") {
      it("should handle zero") {
        Amount.zero.toString should equal("0.00")
      }

      it("should handle values with no dollars") {
        Amount(5).toString should equal("0.05")
        Amount(-5).toString should equal("-0.05")
      }

      it("should handle dollars and no cents") {
        Amount(500).toString should equal("5.00")
        Amount(-500).toString should equal("-5.00")
      }

      it("should handle dollars and cents") {
        Amount(109).toString should equal("1.09")
        Amount(-109).toString should equal("-1.09")
      }
    }

    describe("fromString") {
      it("should parse amounts") {
        Amount.fromString("0") should equal(Amount.zero)
        Amount.fromString("0.00") should equal(Amount.zero)
        Amount.fromString("0.00") should equal(Amount.zero)
        Amount.fromString("0.05") should equal(Amount(5))
        Amount.fromString("10") should equal(Amount(1000))
        Amount.fromString("10.00") should equal(Amount(1000))
        Amount.fromString("10.05") should equal(Amount(1005))
      }
    }
  }
}
