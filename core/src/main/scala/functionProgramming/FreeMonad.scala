package DB

import scalaz._
import scalaz.stream.Process.{Get => _}
import scalaz.Free.{await => _, suspend,_}
import scalaz.\/._
import scala.language.postfixOps
import scalaz.concurrent.Task


trait Query

case object all extends Query

trait QueryBuilder[A]

trait DBRecord[R[_], A] {
  def id(a: R[A]): String
  def format(a: R[A]): QueryBuilder[A]
}


/** A record for updates and retrievals of individual objects of type `A`, keyed on a `String` */
case class KeyedRecord[A](id: String, key: A => String)(implicit F: QueryBuilder[A]) {
  val format = F
}
/** List fo Records */
case class ListRecords[A](id: String)(implicit F: QueryBuilder[A]) {
  val format = F
}

object DBRecord {
  /** Creates a keyed resource for objects of type `T` represented as JSON root objects */
  implicit def keyedRecord[T]: DBRecord[KeyedRecord, T] = new DBRecord[KeyedRecord,T] {
    def id(a: KeyedRecord[T]) = a.id
    def format(a: KeyedRecord[T]) = a.format
  }

  /** Creates a list resource for lists of objects of type `T` represented as JSON arrays */
  implicit def listRecord[T]: DBRecord[ListRecords, T] = new DBRecord[ListRecords, T] {
    def id(a: ListRecords[T]) = a.id
    def format(a: ListRecords[T]) = a.format
  }
}

/** The algebra of CRUD operations */
sealed trait CRUD[+A] {
  def map[B](f: A => B): CRUD[B]
}

case class Create[I,A](a: I, r: KeyedRecord[I], k: Option[DBError] => A) extends CRUD[A] {
  def map[B](f: A => B): CRUD[B] = copy(k = k andThen f)
}

case class Retrieve[I,A](id: String, r: KeyedRecord[I], k: Option[I] => A) extends CRUD[A] {
  def map[B](f: A => B): CRUD[B] = copy(k = k andThen f)
}

case class Update[I,A](upsert: Boolean, a: I, r: KeyedRecord[I], k: Option[DBError] => A) extends CRUD[A] {
  def map[B](f: A => B): CRUD[B] = copy(k = k andThen f)
}

case class Delete[I,A](id: String, r: KeyedRecord[I], k:  Option[DBError] => A) extends CRUD[A] {
  def map[B](f: A => B): CRUD[B] = copy(k = k andThen f)
}

/** A pagination context. The current page elements, with links forwards and back. */
case class Page[A](bck: Option[String], items: List[A], fwd: Option[String])

case class Paginate[I,A](query: Query,
                         r: ListRecords[I],
                         start: Option[String],
                         limit: Option[Int],
                         k: Page[I] => A) extends CRUD[A] {
  def map[B](f: A => B): CRUD[B] = copy(k = k andThen f)
}

case class NextPage[I,A](p: Page[I],
                         r: ListRecords[I],
                         k: Option[Page[I]] => A) extends CRUD[A] {
  def map[B](f: A => B): CRUD[B] = copy(k = k andThen f)
}

case class PrevPage[I,A](p: Page[I],
                         r: ListRecords[I],
                         k: Option[Page[I]] => A) extends CRUD[A] {
  def map[B](f: A => B): CRUD[B] = copy(k = k andThen f)
}

case class ErrorOp[A](err: DBError) extends CRUD[A] {
  def map[B](f: A => B): CRUD[B] = ErrorOp(err)
}

case class DBError(msg: String)

object CRUD {
  implicit val DbOpFunctor: Functor[CRUD] = new Functor[CRUD] {
    def map[A,B](r: CRUD[A])(f: A => B) = r map f
  }
}

object DB {
  import scalaz.stream.Process._

  type DB[A] = Free[CRUD, A]

  /** Wrap a value in `DB`. Monadic unit for the `DB` monad. */
  def apply[A](a: A): DB[A] = point[CRUD,A](a)


  private def embed[A](k: (A => DB[A]) => CRUD[DB[A]]): DB[A] =
    liftF(k(point(_))).flatMap(identity)

  /// Public API ///

  /** Create. */
  def create[A:KeyedRecord](a: A): DB[Unit] =
    embed[Option[DBError]](Create(a, implicitly[KeyedRecord[A]], _)).flatMap(
      _.map(e => error[Unit](e.msg)).getOrElse(DB(())))

  /** Query for a single object by ID */
  def retreive[A: KeyedRecord](id: String): DB[Option[A]] =
    liftF(Retrieve(id, implicitly[KeyedRecord[A]], (x: Option[A]) => x))

  /** Post an update to the given object. */
  def update[A:KeyedRecord](a: A): DB[Unit] =
    embed[Option[DBError]](Update(true, a, implicitly[KeyedRecord[A]], _)).flatMap(
      _.map(e => error[Unit](e.msg)).getOrElse(DB(())))

  def delete[A: KeyedRecord](id: String): DB[Unit] =
    embed[Option[DBError]](Delete(id, implicitly[KeyedRecord[A]], _)).flatMap(
      _.map(e => error[Unit](e.msg)).getOrElse(DB(())))

  /** Query for a single page from a list resource */
  def getPage[A](query: Query,
                 limit: Option[Int] = Some(10),
                 start: Option[String] = None)(implicit R: ListRecords[A]): DB[Page[A]] =
    liftF(Paginate(query, R, start, limit, (x: Page[A]) => x))

  /** Get the next page given a current page, or None if this is the last page */
  def nextPage[A: ListRecords](p: Page[A]): DB[Option[Page[A]]] =
    liftF(NextPage(p, implicitly[ListRecords[A]], (x: Option[Page[A]]) => x))

  /** Get the previous page given a current page or None if this is the first page */
  def prevPage[A: ListRecords](p: Page[A]): DB[Option[Page[A]]] =
    liftF(PrevPage(p, implicitly[ListRecords[A]], (x: Option[Page[A]]) => x))

  /**
   * Query for all objects matching the given query.
   * Returns a stream of pages, one page of `limit` results at a time.
   */
  def getAll[A](query: Query = all,
                limit: Int = 10)(implicit R: ListRecords[A]): scalaz.stream.Process[DB, List[A]] = {
    def emitPage(p: Page[A]): scalaz.stream.Process[DB, List[A]] =
      emit(p.items) ++ p.fwd.map(_ => await(nextPage(p))(_.map(emitPage).getOrElse(halt))).getOrElse(halt)
    await(getPage(query, Some(limit)))(emitPage)
  }

  def getSinglePage[A](query: Query = all,
                       limit: Int = 10)(implicit R: ListRecords[A]): scalaz.stream.Process[DB, List[A]] = {
    def emitPage(p: Page[A]): scalaz.stream.Process[DB, List[A]] =
      emit(p.items) ++ halt
    await(getPage(query, Some(limit)))(emitPage)
  }

  /** A DB operation that always fails with the given error */
  def error[A](msg: String): DB[A] = liftF(ErrorOp(DBError(msg)))

  /** If the operation terminates in an error, catch that error and return it as a value. */
  def attempt[A](r: DB[A]): DB[DBError \/ A] = r.resume.fold({
    case ErrorOp(e) => point(left(e))
    case s => liftF(s) flatMap attempt
  }, a => point(right(a)))
}

class TestDB {
  import DB._

  private val step: CRUD ~> Task = new (CRUD ~> Task) {
    def apply[A](c: CRUD[A]) = op(c)
  }


  /** Lifts a stream of `A` in `DB` to a stream of `A` in `Task` */
  def process[A](p: scalaz.stream.Process[DB, A]): scalaz.stream.Process[Task, A] =
    p.translate(trans)

  /** A natural transformation from `DBAction` to `Task` */
  val trans: DB ~> Task = new (DB ~> Task) {
    def apply[A](c: DB[A]) = run(c)
  }


  /** Turn the `DB` script into a `Task` that executes a set of DB requests when run. */
  def run[A](action: DB[A]): Task[A] =
    action.foldMap(step)

  /** An interpreter of `DB` actions into `Task` actions that perform DB Actions when run. */
  private def op[A](c: CRUD[A]): Task[A] = {
    c match {
      case Create(a, r, k) => sys.error("Not Implemented Yet")

      case Retrieve(id, r, k) =>  sys.error("Not Implemented Yet")

      case Update(u,id,r,k) => sys.error("Not Implemented Yet")

      case Delete(id,r,k) => sys.error("Not Implemented Yet")

      case Paginate(q, r, start, limit, k) => sys.error("Not implement yet")

      case NextPage(p, r, k) => sys.error("Not implement yet")

      case PrevPage(p, r, k) => sys.error("Not implement yet")

      case ErrorOp(e) => Task.fail(new Exception(e.msg))
    }
  }
}
