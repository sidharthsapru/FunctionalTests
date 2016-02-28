case class State[S,A](run: S => (S,A))
case class Id[A](value: A)
case class Reader[R,A](run: R => A)

trait Monad[F[_]] {
  def unit[A](a : => A): F[A]
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
  def map[A,B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a,b)))
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))
  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la map f)
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = 
    ms.foldLeft(unit(List[A]()))((mla,ma) => 
        flatMap(f(ma))(a => if(a) mla else map2(unit(ma),mla)(_ :: _) ))
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)
  def flatMapWithCompose[A,B](fa: F[A])(f: A =>F[B]): F[B] = compose((_: Unit) => fa, f)(())
  def join[A](mma: F[F[A]]): F[A]= flatMap(mma)(ma => ma)
  def flatMapWithJoin[A,B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
}
object Monad {

  val listMonad = new Monad[List] {
    def unit[A](a: => A) = List(a)
    def flatMap[A,B](fa: List[A])(f: A => List[B]): List[B] = flatMap(fa)(f)
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A) = Some(a)
    def flatMap[A,B](fa: Option[A])(f: A => Option[B]): Option[B] = flatMap(fa)(f)
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A) = Stream(a)
    def flatMap[A,B](fa: Stream[A])(f: A => Stream[B]): Stream[B] = flatMap(fa)(f)
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S,x]})#f] {
    def unit[A](a: => A):State[S,A] = State(s => (s,a))
    def flatMap[A,B](fa: State[S,A])(f: A => State[S,B]): State[S,B] = State(s => {
      val (s1,a) = fa.run(s)
      val s2 = f(a)
      s2.run(s1)
    })
  }

  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = Id(a)
    def flatMap[A,B](fa: Id[A])(fb: A => Id[B]): Id[B] = fb(fa.value)
  }
  
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = Reader(r => a)
    def flatMap[A,B](rm: Reader[R,A])(f: A => Reader[R,B]) = Reader(r => {
      val a = rm.run(r)
      f(a).run(r)
    })
  }

}
