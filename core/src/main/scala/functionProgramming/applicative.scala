trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def apply[A,B](f: F[A => B])(fa: F[A]): F[B]
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = {
    val f1 = (a:A) => (b:B) => f(a,b)
    val f2 = map(fa)(f1)
    apply(f2)(fb)
  }

  def map[A,B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a,mla) => map2(f(a), mla)(_ :: _))
  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(a => a)

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = 
    ofa.foldLeft(unit(Map[K,V]()))( (mla,kfv) => 
        map2(mla,kfv._2)((a,b) => a + (kfv._1 -> b)))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))
  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa,fb)((a,b) => (a,b))
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      def apply[A,B](fa: (F[A => B], G[A =>B]))(fga: (F[A], G[A])): (F[B],G[B]) =
        (self.apply(fa._1)(fga._1), G.apply(fa._2)(fga._2))
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f]{
      def unit[A](a: => A) = self.unit(G.unit(a))
      def apply[A,B](fga: F[G[A => B]])(fa: F[G[A]]): F[G[B]] = self.map2(fga,fa)((gab,ga) =>
          G.map2(gab,ga)((f,a) => f(a)))
      override def map2[A,B,C](fga: F[G[A]], fgb: F[G[B]])(f: (A,B) => C) = self.map2(fga,fgb)(G.map2(_,_)(f))
    }
  }

}

sealed trait Validation[+E,+A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative { 
  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E,x]})#f] {
    def unit[A](a: => A) = Success(a)
    override def map2[A,B,C](fa: Validation[E,A], fb: Validation[E,B])(f: (A,B) => C): Validation[E,C] = (fa,fb) match {
      case (Success(a), Success(b)) => unit(f(a,b))
      case (Success(a), Failure(e,t)) => Failure(e,t)
      case (Failure(e,t),Success(a)) => Failure(e,t)
      case (Failure(a,t1), Failure(b,t2)) => Failure(b,Vector(a) ++ t1 ++ t2)
    }

    override def apply[A,B](f: Validation[E,A => B])(fa: Validation[E,A]): Validation[E,B] = (fa,f) match {
      case (Success(a),Success(f1)) => unit(f1(a))
      case (Success(a), Failure(e,t)) => Failure(e,t)
      case (Failure(e,t), Success(f1)) => Failure(e,t)
      case (Failure(e1,t1), Failure(e2,t2)) => Failure(e1, Vector(e2) ++  t1 ++ t2)
    }
  }
}




