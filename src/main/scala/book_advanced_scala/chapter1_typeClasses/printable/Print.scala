package book_advanced_scala.chapter1_typeClasses.printable


object Print {
  def format[A](value: A)(implicit printable: Printable[A]): String =
    printable.format(value)

  def print[A](value: A)(implicit printable: Printable[A]): Unit = println(printable.format(value))
}
