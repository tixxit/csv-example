package csv
package examples

import shapeless._

object AddressBook { //extends App {
  case class Address(street: String, city: String, prov: String)
  case class Person(name: String, age: Int, address: Address)

  implicit val addressIso = Iso.hlist(Address.apply _, Address.unapply _)
  implicit val personIso = Iso.hlist(Person.apply _, Person.unapply _)

  val addressBook = List(
    Person("Amanda", 27, Address("123 Made-up Way", "Calgary", "AB")),
    Person("Bob", 32, Address("4 Crummy St, Apt B", "Toronto", "ON")),
    Person("Charlie", 7, Address("2 Rue Deux", "Montreal", "QC")),
    Person("Eric", 67, Address("22 Wallaby Way", "Sydney", "NS")),
    Person("Tom", 65, Address("5555 Blah St", "Huntsville", "ON")),
    Person("Zed", 42, Address("123 Abc Dr", "Vancouver", "BC")))

  // SepValues[Person] // Not good.
  // SepValues[Address :: HNil] // Good.
  // SepValues.hlist[Int, Address :: HNil] // Not good.
  // SepValues.hlist[Int, Address :: HNil](implicitly, implicitly) // Good!?!?
  // SepValues[Address :: Int :: HNil] // Good.
  // SepValues.hlist[Int, (String :: String :: String :: HNil) :: HNil] // Good.
  // SepValues.appender[Address, HNil,
  //       String :: String :: String :: HNil,
  //       String :: String :: String :: HNil,
  //       Nat._3] // Good.

  // Doesn't work:
  // val rows = addressBook map { p: Person =>
  //   Format.CSV.toRow[Person](p)
  // }
  // println(rows mkString "\n")
}
