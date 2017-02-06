package com.capgemini

import com.capgemini.Checkout._
import org.scalacheck.{Gen, Arbitrary}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

/**
  * Created by max on 06/02/17.
  */
class CheckoutSpec extends WordSpec with Matchers with PropertyChecks{

  val checkout = new Checkout

  "The checkout system can parse input" when {
    "the input is correctly formatted and output appropriate case objects" in {
      val output = checkout.parseInput("Apple" :: "Apple" :: "Apple" :: "Orange" :: "Orange" :: Nil)
      output shouldEqual Right(List(Apple,Apple,Apple,Orange,Orange))
    }
    "the input is badly formatted and output an error string" when {
      "the invalid input is at the front of the input" in {
        val output = checkout.parseInput("Oops" :: "Apple" :: "Apple" :: "Orange" :: "Orange" :: Nil)
        output shouldEqual Left("Invalid input: Oops")
      }
      "the invalid input is at the end of the input" in {
        val output = checkout.parseInput("Apple" :: "Apple" :: "Orange" :: "Orange" :: "Pear" :: Nil)
        output shouldEqual Left("Invalid input: Pear")
      }
    }
  }

  "The checkout system can add up fruits correctly" in {
    checkout.calculate(List(Apple,Apple,Apple)) shouldEqual 120

    checkout.calculate(List(Orange,Apple,Orange)) shouldEqual 110

    checkout.calculate(List(Orange,Orange,Orange)) shouldEqual 50
  }

  "The checkout system can add up arbitrary large lists correctly" in {

    val fruitGen:Gen[Fruit] = Gen.oneOf(Apple,Orange)
    implicit val arbFruit = Arbitrary { fruitGen }

    forAll { (shoppingList: List[Fruit]) ⇒
      val numApples = shoppingList.count{
        case Apple ⇒ true
        case _     ⇒ false
      }


      val numOranges = shoppingList.size - numApples

      val adjustedNumApples: Int = Math.ceil(numApples / 2.0).toInt

      val adjustedNumOranges: Int = (numOranges / 3 * 2) + (numOranges % 3)

      val total = (adjustedNumApples * 60) + (adjustedNumOranges * 25)

      checkout.calculate(shoppingList) shouldEqual total
    }
  }

  "The checkout system can take a list of items " when {
    "the input is correctly formatted it outputs the cost in pence" in {
      val output = checkout.scan("Apple" :: "Apple" :: "Apple" :: "Orange" :: "Orange" :: Nil)
      output shouldEqual Right(170)
    }
    "the input is badly formatted it outputs an error" in {
      val output = checkout.scan("Oops" :: "Apple" :: "Apple" :: "Orange" :: "Orange" :: Nil)
      output shouldEqual Left("Invalid input: Oops")
    }
  }




}
