package org.autoTdd.transformation

import scala.collection.immutable.List

import org.autoTdd.engine.Engine1

//dynamic types might rock here
abstract class Currency
object GBP extends Currency

case class Library(val books: List[Book])
object Library{
  def apply(books: Book*) =new  Library(books)
}
case class Book(val name: String, val price: Double, val currency: Currency)

trait BooksForTest {
  
  protected val harryPotterBook = Book("Philosopher's stone", 4.50, GBP)
  protected val blackSwanBook = Book("Black Swan", 5.00, GBP)

  protected val harryPotterMap = Map[String, Object]("name" -> "Philosopher's stone", "price" -> double2Double(4.50), "currency" -> "GBP")
  protected val blackSwanMap = Map[String, Object]("name" -> "Black Swan", "price" -> double2Double(5.00), "currency" -> "GBP")

}

object LibraryToJson extends BooksForTest {
  val libraryTransformer = Engine1[Library, Map[String, Object]](Map());
  val bookTransformer = Engine1[Book, Map[String, Object]](Map());

  libraryTransformer.constraint(Library(List(harryPotterBook, blackSwanBook)), Library(harryPotterMap,blackSwanMap ), code, because)

}

object JsonToLibrary {

  val libraryTransformer = Engine1[Map[String, Object], Library](Library(List()));
  val bookTransformer = Engine1[Map[String, Object], Book](null);

}