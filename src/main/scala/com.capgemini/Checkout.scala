package com.capgemini

import com.capgemini.Checkout.{Fruit, Apple, Orange}

/**
  * Checkout class encompassing all the methods and type aliases used for the
  * Shopping Cart system
  */
class Checkout {

  type ErrorString = String

  type Price = Int

  val pricePerOrange:Price = 25

  val pricePerApple:Price = 60

  /**
    * Turn an input list of strings into either an ErrorString or a Price
    */
  private[capgemini] def scan(list:List[String]) : Either[ErrorString,Price] =
    parseInput(list).map(calculate)

  /**
    * Turn an input String into either an ErrorString or a Fruit
    */
  private[capgemini] def stringToFruit(s:String) : Either[ErrorString,Fruit] =
    s.trim.toLowerCase match {
      case "apple"  ⇒ Right(Apple)
      case "orange" ⇒ Right(Orange)
      case other    ⇒ Left(s"Invalid input: $s")
    }

  /**
    * Turn a list of input strings into either an ErrorString or a List of Fruits
    */
  private[capgemini] def parseInput(input:List[String]) : Either[ErrorString,List[Fruit]] =
    input.foldRight(Right(Nil):Either[ErrorString,List[Fruit]])((s,acc) ⇒
      stringToFruit(s).flatMap(f ⇒ acc.map(f :: _))
    )

  /** ThreeForTwo offer for oranges */
  private[capgemini] def orangeOffer(oranges: List[Orange.type]) : Price =
    (oranges.size / 3 * 2 + oranges.size % 3) * pricePerOrange

  /** Buy one get one free apple offer */
  private[capgemini] def appleOffer(apples: List[Apple.type]) : Price =
    Math.ceil(apples.size / 2.0).toInt * pricePerApple

  /**
    * Given a list of fruits calculate the price
    */
  private[capgemini] def calculate(input:List[Fruit]) : Price = {
    val (oranges, apples) = input.foldLeft((List.empty[Orange.type],List.empty[Apple.type]))((acc, f) ⇒
     f match {
       case Orange ⇒ (Orange :: acc._1) → acc._2
       case Apple  ⇒ acc._1 → (Apple :: acc._2)
     }
    )
    orangeOffer(oranges) + appleOffer(apples)
  }

}

object Checkout {

  sealed trait Fruit
  case object Apple extends Fruit
  case object Orange extends Fruit

}
