package DB

trait DBResource[R[_], A] {
  def format(a: R[A]): JsonFormat[A]
}

/** A resource for updates and retrievals of individual objects of type `A`, keyed on a `ObjectId` */
case class KeyedResource[A](key: A => ObjectId)(implicit F: RootJsonFormat[A]) {
  val format = F
}

/** A resource for retrievals of lists and pages of objects of type `A` */
case class ListResource[A](implicit F: JsonFormat[A]) {
  val format = F
}

object Resource {
  /** Creates a keyed resource for objects of type `T` represented as JSON root objects */
  implicit def keyedJsonResource[T]: DBResource[KeyedResource, T] = new DBResource[KeyedResource, T] {
    def format(a: KeyedResource[T]) = a.format

  }

  /** Creates a list resource for lists of objects of type `T` represented as JSON arrays */
  implicit def listJsonResource[T]: DBResource[ListResource, T] = new DBResource[ListResource, T] {
    def format(a: ListResource[T]) = a.format
  }
}

sealed trait CRUD[+A] {
  def map[B](f: A => B): CRUD[B]
}

case class Retrieve[I,A](query: JsValue, r: KeyedResource[I], k: Option[I] => A) extends CRUD[A] {
  def map[B](f: A => B): CRUD[B] = copy(k = k andThen  f)
}

case class Page[A](bck: Option[ObjectId], items: List[A], fwd: Option[ObjectId], query: JsValue, limit: Option[Int])

case class Paginate[I,A](query: JsValue,
                         r: ListResource[I],
                         limit: Option[Int],
                         start: Option[ObjectId],
                         k: Page[I] => A) extends CRUD[A] {
  def map[B](f: A => B): CRUD[B] = copy(k = k andThen f)
}

case class NextPage[I,A](query: JsValue, r: ListResource[I], start:Option[ObjectId], limit: Option[Int],
                         k: Option[Page[I]] => A) extends CRUD[A] {
  def map[B](f: A => B): CRUD[B] = copy(k = k andThen f)
}

case class PrevPage[I,A](query: JsValue,r: ListResource[I], start:Option[ObjectId], limit: Option[Int],
                         k: Option[Page[I]] => A) extends CRUD[A] {
  def map[B](f: A => B): CRUD[B] = copy(k = k andThen f)
}

case class WriteOne[I,A](upsert: Boolean, a: I, r: KeyedResource[I], k: Option[DBError] => A) extends CRUD[A] {
  def map[B](f: A => B): CRUD[B] = copy(k = k andThen f)
}

case class UpdateOne[I,A](query: JsValue, a: I, r: KeyedResource[I], k: Option[DBError] => A) extends CRUD[A] {
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

  type DB[A] = Free[CRUD, A]

  /** Wrap a value in `DB`. Monadic unit for the `DB` monad. */
  def apply[A](a: A): DB[A] = point[CRUD,A](a)

  /** Query for an object */
  def retrieve[A: KeyedResource](query: JsValue): DB[Option[A]] =
    liftF(Retrieve(query, implicitly[KeyedResource[A]], (x: Option[A]) => x))


  /** Query for a single page from a list resource */
  def getPage[A: ListResource](query: JsValue,
                 limit: Option[Int] = Some(10),
                 start: Option[ObjectId] = None): DB[Page[A]] =
    liftF(Paginate(query,implicitly[ListResource[A]],limit,start, (x: Page[A]) => x))

  /** Get the next page given a current page, or None if this is the last page */
  def nextPage[A: ListResource](p: Page[A]): DB[Option[Page[A]]] =
    liftF(NextPage(p.query,implicitly[ListResource[A]],p.fwd, p.limit,(x: Option[Page[A]]) => x))

  /** Get the previous page given a current page or None if this is the first page */
  def prevPage[A: ListResource](p: Page[A]): DB[Option[Page[A]]] =
    liftF(PrevPage(p.query,implicitly[ListResource[A]],p.bck, p.limit,(x: Option[Page[A]]) => x))

  /**
   * Query for all objects matching the given query.
   * Returns a stream of pages, one page of `limit` results at a time.
   */
  def getAll[A](query: JsValue = JsObject(),limit: Int = 10)(implicit R: ListResource[A]): Process[DB, List[A]] = {
    def emitPage(p: Page[A]): Process[DB, List[A]] =
      emit(p.items) ++ p.fwd.map(_ => await(nextPage(p))(_.map(emitPage).getOrElse(halt))).getOrElse(halt)
    await(getPage(query, Some(limit)))(emitPage)
  }


  private def embed[A](k: (A => DB[A]) => CRUD[DB[A]]): DB[A] =
    liftF(k(point(_))).flatMap(identity)

  /** Post an update to the given object. */
  def update[A:KeyedResource](a: A): DB[Unit] =
    embed[Option[DBError]](WriteOne(true, a, implicitly[KeyedResource[A]], _)).flatMap(
      _.map(e => error[Unit](e.msg)).getOrElse(DB(())))

  /** Find and Modify a given object. */
  def updateOne[A:KeyedResource](query: JsValue, a: A): DB[Unit] =
    embed[Option[DBError]](UpdateOne(query,a, implicitly[KeyedResource[A]], _)).flatMap(
      _.map(e => error[Unit](e.msg)).getOrElse(DB(())))


  /** Create or replace the given object. */
  def create[A:KeyedResource](a: A): DB[Unit] =
    embed[Option[DBError]](WriteOne(false, a, implicitly[KeyedResource[A]], _)).flatMap(
      _.map(e => error[Unit](e.msg)).getOrElse(DB(())))

  /** A DB operation that always fails with the given error */
  def error[A](msg: String): DB[A] = liftF(ErrorOp(DBError(msg)))

  /** If the operation terminates in an error, catch that error and return it as a value. */
  def attempt[A](r: DB[A]): DB[DBError \/ A] = r.resume.fold({
    case ErrorOp(e) => point(left(e))
    case s => liftF(s) flatMap attempt
  }, a => point(right(a)))


}

trait Repository {
  import DB._


  def op[A](c: CRUD[A]): Task[A]

  private val step: CRUD ~> Task = new (CRUD ~> Task) {
    def apply[A](c: CRUD[A]) = op(c)
  }

  /** A natural transformation from `DBAction` to `Task` */
  private val trans: DB ~> Task = new (DB ~> Task) {
    def apply[A](c: DB[A]) = run(c)
  }


  /** Turn the `DB` script into a `Task` that executes a set of DB requests when run. */
  def run[A](action: DB[A]): Task[A] =
    action.foldMap(step)
}
