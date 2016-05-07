package book_advanced_scala.chapter1_typeClasses.printable

object PrintSyntax {
  implicit class PrintOps[A](value: A) {
    def format(implicit printable: Printable[A]): String =
      printable.format(value)

    def print(implicit printable: Printable[A]): Unit = println(printable.format(value))
  }
}
