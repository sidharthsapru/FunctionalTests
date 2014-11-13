package functionalProgramming
import java.util.concurrent._
object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  def unit[A](a: A) : Par[A] = (es: ExecutorService) => UnitFuture[A](a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] { 
      def call = a(es).get
    })

  def map2[A,B,C](a: Par[A], b:Par[B])(f:(A,B) => C): Par[C] = (es:ExecutorService) => {
    val a1 = a(es)
    val b1 = b(es)
    UnitFuture(f(a1.get,b1.get))
  }

  def map[A,B](a: Par[A])(f: A => B): Par[B] = map2(a,unit(()))((a,_) => f(a))

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    val p = run(es)(n).get
    run(es)(choices(p))
  }

  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => {
    run(es)(choices(run(es)(pa).get))
  }

  def join[A](a: Par[Par[A]]): Par[A] = es => {
    val v: Par[A] = run(es)(a).get()
    run(es)(v)
  }

  def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] = {
    join(map(a)(f))
  }

}
