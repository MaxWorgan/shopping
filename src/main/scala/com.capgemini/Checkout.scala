package com.capgemini

import com.capgemini.Checkout.{Fruit, Apple, Orange}

/**
  * Checkout class encompassing all the methods and type aliases used for the
  * Shopping Cart system
  */
class Checkout {

  type ErrorString = String

  type Price = Int

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

  /**
    * Given a list of fruits calculate the price
    */
  private[capgemini] def calculate(input:List[Fruit]) : Price = input.foldLeft(0)((acc,f) ⇒ f match {
    case Apple ⇒ acc + 60
    case Orange ⇒ acc + 25
  })

}

object Checkout {

  sealed trait Fruit
  case object Apple extends Fruit
  case object Orange extends Fruit

}
