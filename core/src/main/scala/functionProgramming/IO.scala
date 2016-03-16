trait Monad[F[_]] {
  def unit[A](a : => A): F[A]
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
  def forever[A,B](fa: F[A]): F[B] = {
    lazy val t: F[B] = forever(fa)
    flatMap(fa)(_ => t)
  }
}

object IO {
  trait IO[A] {
    def map[B](f: A => B): IO[B] =  flatMap(f andThen (Return(_)))
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this,f)
  }
  case class Return[A](a: A) extends IO[A]
  case class Suspend[A](resume: () => A) extends IO[A]
  case class FlatMap[A,B](sub: IO[A], k: A => IO[B]) extends IO[B]

  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = Return(a)
    def flatMap[A,B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f

    @annotation.tailrec
    def run[A](io: IO[A]): A = io match {
      case Return(a) => a
      case Suspend(f) => f()
      case FlatMap(x,f) => x match {
        case Return(a) =>  run(f(a))
        case Suspend(r) => run(f(r()))
        case FlatMap(y,g) => run((y flatMap g flatMap f))
      }
    }
  }
}

object TailRec {
  trait TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this,f)
    def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))
  }

  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A,B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]


  @annotation.tailrec
  def run[A](tail: TailRec[A]): A = tail match {
    case Return(a) => a
    case Suspend(f) => f()
    case FlatMap(x,f) => x match {
     case Return(a) =>  run(f(a))
     case Suspend(r) => run(f(r()))
     case FlatMap(y,g) => run((y flatMap g flatMap f))
   }
 }
}


object Free {
  trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F,B]): Free[F,B] = FlatMap(this,f)
    def map[B](f: A => B): Free[F,B] = flatMap(f andThen (Return(_)))
  }
  case class Return[F[_],A](a: A) extends Free[F,A]
  case class Suspend[F[_],A](s: F[A]) extends Free[F,A]
  case class FlatMap[F[_],A](sub: Free[F,A], f: A => Free[F,B]) extends Free[F,A]

  def freeMonad[F[_]]: Monad[({type f[a] = Free[F,A]})#f] = new Monad[({type f[a] = Free[F,A]})#f] {
    def flatMap[B](fa: Free[F,A])(f: A => Free[F,B]]): Free[F,B] = fa flatMap f
    def unit(a: => A) = Return(a)
  }

  @annotation.tailrec
  def runTrampoline[A](a: Free[Funtion0,A]): A = a match {
    case Return(a) => a
    case Suspend(f) => f()
    case FlatMap(x,f) => x match {
      case Return(a) =>  run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y,g) => run((y flatMap g flatMap f))
    }
  }

  def run[F[_],A](a: Free[F,A])(implicit F: Monad[F]): F[A] = a match {
    case Return(a) => F.point(a)
    case Suspend(f) => F.point(f())
    case FlatMap(x,f) => x match {
      case Return(a) => run(f(a))(F)
      case Suspend(r) => run(f(r()))(F)
      case FlatMap(y,g) => ???
    }
  }
}


