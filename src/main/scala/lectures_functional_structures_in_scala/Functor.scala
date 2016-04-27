package lectures_functional_structures_in_scala

trait Functor[F[_]] {   //type class that abstracts over type constructor, or type constructor of one argument
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait FunctorLaws{
  def identity[F[_], A](fa: F[A])(implicit F: Functor[F]) =
    F.map(fa)(a => a) == fa

  def composition[F[_], A, B, C](fa: F[A], f: A => B, g: B => C)(implicit F: Functor[F]) =
    F.map(F.map(fa)(f))(g) == F.map(fa)(f andThen g)
}


