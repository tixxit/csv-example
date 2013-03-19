package csv
package examples

import Format.CSV

import shapeless._

object AddressBook extends App {
  final case class Address(street: String, region: String)
  final case class Person(name: String, age: Int, address: Address)

  implicit val addressIso = Iso.hlist(Address.apply _, Address.unapply _)
  implicit val personIso = Iso.hlist(Person.apply _, Person.unapply _)

  val book = List(
    Person("Amanda", 27, Address("123 Made-up Way", "Calgary, AB")),
    Person("Bob", 32, Address("4 Crummy St, Apt B", "Toronto, ON")),
    Person("Charlie", 7, Address("2 Rue Deux", "Montreal, QC")),
    Person("Eric", 67, Address("22 Wallaby Way", "Sydney, NS")),
    Person("Tom", 65, Address("5555 Blah St", "Huntsville, ON")),
    Person("Zed", 42, Address("123 Abc Dr", "Vancouver, BC")))

  val csv = book map (CSV.toRow(_)) mkString "\n"
  val book2 = (csv split "\n" flatMap (CSV.fromRow[Person](_))).toList

  println(s"CSV:\n$csv\n\n")
  println(s"Data:\n$book2\n\n")
}
