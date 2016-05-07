package book_advanced_scala.chapter1_typeClasses.printable

trait Printable[A] {
  def format(value: A): String
}

object PrintDefaults {

  implicit object PrintableString extends Printable[String] {
     def format(value: String): String = value
  }

  implicit object PrintableInt extends Printable[Int] {
    def format(value: Int): String = value.toString
  }

  implicit object PrintableCat extends Printable[Cat] {
    def format(cat: Cat): String = s"${cat.name }is a ${cat.age} year-old ${cat.color} cat."
  }
}



final case class Cat(name: String, age: Int, color: String)
