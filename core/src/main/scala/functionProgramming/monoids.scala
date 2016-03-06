trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    def zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    def zero = Nil
  }
  val intMonoid = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2:Int): Int = a1*a2
    def zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
    def zero = None
  }

  def endoMonoid[A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 compose a2
    def zero = (a: A) => a
  }

  def Ordered(as: IndexedSeq[Int]): Boolean = {
    val getBiggest = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = if(a1 < a2) a2 else a1
    def zero = Integer.MIN_VALUE
  }
  val p = foldMap[Int,Int](as.toList,a => a)(getBiggest)
  as.last == p
  }
 
  def concatenate[A](as: List[A])(implicit m: Monoid[A]) = as.foldLeft(m.zero)(m.op)
  def foldMap[A,B](as: List[A], f:A => B)(implicit m: Monoid[B]): B = as.foldLeft(m.zero)((a,b) => m.op(a,f(b)))

  def foldLeft[A,B](as: List[A],z:B)(f: (A,B) => B) = foldMap(as, f.curried)(endoMonoid[B])(z)

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = if(v.length >2) {
    val (v1,v2) = v.splitAt(v.length/2)
    m.op(foldMapV(v1,m)(f), foldMapV(v2,m)(f))
  } else if(v.length == 1) {
    f(v.head)
  } else m.zero

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A,B)] = new Monoid[(A,B)] {
    def op[A,B](a1: (A,B), b1: (A,B)): (A,B) = (A.op(a1._1, b1._1),B.op(a1._2,b1._2))
    def zero = (A.zero,B.zero)
  }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(f1: A => B, f2: A => B) = a => B.op(f1(a),f2(a))
    def zero = a => B.zero
  }
}

sealed trait WC

case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object WC {

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a: WC, b: WC): WC = (a,b) match {
      case(a: Part, b: Part) => Part(a.lStub, if((a.rStub + b.lStub).isEmpty){
        (a.words + b.words)} else {
          (a.words + b.words + 1)}, b.lStub)
      case(a: Part, b: Stub) => Part(a.lStub, a.words, a.rStub + b.chars)
      case(a: Stub, b: Part) => Part(a.chars+b.lStub , b.words, b.rStub)
      case(a: Stub, b: Stub) => Stub(a.chars+b.chars)
    }
    def zero = Stub("")
  }

}


trait Foldable[F[_]]{
  def foldRight[A,B](as: F[A])(z:B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(z:B)(f: (B,A) => B): B
  def foldMap[A,B](as: F[A])(z:B)(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
  def toList[A](fa: F[A]): List[A] = foldLeft(fa)(List[A]())( (b,a) => b::a)
}

sealed trait Tree[+A]
case object Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Foldable {
  val Foldable[List] = new Foldable[List] {
    def foldRight[A,B](as: List[A])(z: B)(f: (A,A) => B): B = foldMap(as)(f.curried)(endoMonoid[B])(z)
    def foldMap[A,B](as: List[A])(f: A => B)(mb: Monoid[B]): B = as.foldMap(f)(mb)
  }

  val Foldable[Tree] = new Foldable[Tree] {
    def foldMap[A,B](as: Tree[A])(z:B)(f: A => B)(mb: Monoid[B]): B = as match {
      case Leaf(value) => f(value)
      case Branch(l,r) = mb.op(foldLeft(l)(z)(f)(mb), foldLeft(r)(z)(f)(mb))
    }
  }
}
