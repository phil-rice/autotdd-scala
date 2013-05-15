package org.autoTdd.transformation

import scala.collection.immutable.List

import org.autoTdd.engine.Engine1
import org.junit.runner.RunWith
import org.autoTdd.engine.tests.AutoTddRunner
import org.autoTdd.engine.Engine2

//dynamic types might rock here
sealed abstract class Currency(val name: String)
object Currency {
  def apply(s: String): Currency =
    s match {
      case "GBP" => GBP
      case _ => throw new IllegalArgumentException(s)
    }
}

object GBP extends Currency("GBP")

case class Book(val name: String, val price: Double, val currency: Currency)
case class Library(val books: List[Book])
object Library {
  def apply(books: Book*) = {
    val fn = books.foldLeft(List[Book]())_
    val list = fn((acc: List[Book], b: Book) => b :: acc)
    new Library(list)
  }
}

trait BooksForTest {

  protected val harryPotterBook = Book("Philosopher's stone", 4.50, GBP)
  protected val blackSwanBook = Book("Black Swan", 5.00, GBP)

  protected val harryPotterMap = Map[String, Object]("name" -> "Philosopher's stone", "price" -> double2Double(4.50), "currency" -> "GBP")
  protected val blackSwanMap = Map[String, Object]("name" -> "Black Swan", "price" -> double2Double(5.00), "currency" -> "GBP")

}

@RunWith(classOf[AutoTddRunner])
object LibraryToJson extends BooksForTest {
  val libraryToJson = Engine1[Library, List[Map[String, Object]]](
    (l: Library) => l.books.map((BookToJson(_))));

  libraryToJson.constraint(Library(List(harryPotterBook, blackSwanBook)), expected = List(harryPotterMap, blackSwanMap))

  val jsonToLibrary = Engine1[List[Map[String, Object]], Library](
    (l: List[Map[String, Object]]) => Library(l.map(BookToJson(_))));
  jsonToLibrary.constraint(List(harryPotterMap, blackSwanMap), Library(List(harryPotterBook, blackSwanBook)))

  def apply(l: Library) = libraryToJson(l)
  def apply(l: List[Map[String, Object]]) = jsonToLibrary(l)
}

@RunWith(classOf[AutoTddRunner])
object BookToJson extends BooksForTest {
  val book2Json = Engine1[Book, Map[String, Object]](
    (b: Book) => Map[String, Object]("name" -> b.name, "price" -> double2Double(b.price), "currency" -> b.currency.name));
  book2Json.constraint(harryPotterBook, harryPotterMap)
  book2Json.constraint(blackSwanBook, blackSwanMap)

  val json2Book = Engine1[Map[String, Object], Book](
    (m: Map[String, Object]) => Book(m("name").asInstanceOf[String], m("price").asInstanceOf[Double], Currency(m("currency").asInstanceOf[String])));
  json2Book.constraint(harryPotterMap, harryPotterBook)
  json2Book.constraint(blackSwanMap, blackSwanBook)

  def apply(b: Book) = book2Json(b)
  def apply(m: Map[String, Object]) = json2Book(m)
}

