package one.xingyi.finatra

import com.twitter.finagle.http.{Method, Request, Response}
import com.twitter.finatra.utils.FuturePools
import com.twitter.util.{Await, FuturePool, Local, Return, Throw, Duration => TDuration, Future => TFuture}
import one.xingyi.core.http._
import one.xingyi.core.local.{Holder, SimpleLocalOps}
import one.xingyi.core.monad.{Async, LocalVariable, MonadCanFailWithException, MonadWithState}

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.Duration
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.{Try => STry}

object AsyncForTwitterFuture {
  val localVariableMap = new TrieMap[LocalVariable[_], Local[_]]()

}
class AsyncForTwitterFuture(implicit futurePool: FuturePool) extends Async[TFuture] with MonadCanFailWithException[TFuture, Throwable] with MonadWithState[TFuture] {
  override def async[T](t: => T) = futurePool(t)
  override def respond[T](m: TFuture[T], fn: STry[T] => Unit) = m.respond(tryS => fn(tryS.asScala))
  override def await[T](m: TFuture[T]) = Await.result(m, TDuration.fromSeconds(5))
  override def delay[T](duration: Duration)(block: => TFuture[T]) = ???
  override def foldWithExceptionAndFail[T, T1](m: TFuture[T], fnE: Throwable => TFuture[T1], fnFailure: Throwable => TFuture[T1], fn: T => TFuture[T1]) = m.transform {
    case Return(t) => fn(t)
    case Throw(t) => fnE(t)
  }
  override def exception[T](t: Throwable) = TFuture.exception(t)
  override def recover[T](m: TFuture[T], fn: Throwable => TFuture[T]): TFuture[T] = m.rescue { case e: Throwable => fn(e) }
  override def flatMapEither[T, T1](m: TFuture[T], fn: Either[Throwable, T] => TFuture[T1]): TFuture[T1] = {
    m.transform {
      case Return(t) => fn(Right(t))
      case Throw(t) =>
        fn(Left(t))
    }
  }
  override def flatMap[T, T1](m: TFuture[T], fn: T => TFuture[T1]) = m.flatMap(fn)
  override def map[T, T1](m: TFuture[T], fn: T => T1) = m.map(fn)
  override def fail[T](f: Throwable) = TFuture.exception(f)
  override def liftM[T](t: T) = TFuture.value(t)


  def getLocal[V](localVariable: LocalVariable[V]) = AsyncForTwitterFuture.localVariableMap.getOrElseUpdate(localVariable, new Local[Seq[V]]()).asInstanceOf[Local[Seq[V]]]

  override def putInto[V, T](localVariable: LocalVariable[V], t: V)(m: TFuture[T]): TFuture[T] = m.map { x =>
    val local = getLocal(localVariable)
    println(s"putInto: $x $local $this ${AsyncForTwitterFuture.localVariableMap}")
    val oldSeq = local().getOrElse(Seq())
    println(s"  oldSeq: $oldSeq")
    local.update(oldSeq ++ Seq(t))
    println(s"  new: ${local()}")
    println(s"  new: ${getLocal(localVariable)()}")
    x
  }
  override def mapState[V, T, T1](m: TFuture[T], localVariable: LocalVariable[V], fn: Seq[V] => T1): TFuture[T1] = m.map { x => fn(getLocal(localVariable)().getOrElse(Seq())) }
  override def mapWith[V, T, T1](m: TFuture[T], localVariable: LocalVariable[V], fn: (T, Seq[V]) => T1): TFuture[T1] = m.map { x =>
    val local = getLocal(localVariable)
    println(s"in mapwith $local $this   ${AsyncForTwitterFuture.localVariableMap}")
    println(s"   in mapwith ${local()}")
    val seq = local().getOrElse(Seq())
    println(s"  in mapwith - $seq")
    fn(x, seq)
  }
}

object FinatraImplicits {

  val localHolder = new Holder[Local] {
    override def makeHolder[V: ClassTag]: Local[V] = new Local()
    override def getValueOutOfHolder[V](holder: Local[V]): Option[V] = holder()
    override def putValueInHolder[V](v: Option[V])(holder: Local[V]): Unit = holder.set(v)
  }

  implicit val localOps = new SimpleLocalOps[TFuture, Local](localHolder)
  object ImplicitsForTest {
    implicit val futurePool = FuturePools.fixedPool("Future pool for tests", 20)
    implicit val asyncForTwitter = new AsyncForTwitterFuture

  }

  implicit object ToServiceResponseForFinatraResponse extends ToServiceResponse[Response] {
    override def apply(response: Response): ServiceResponse = ServiceResponse(Status(response.statusCode), Body(response.contentString), ContentType(response.mediaType.getOrElse("")))
  }

  implicit object ToServiceRequest extends ToServiceRequest[Request] {
    override def apply(request: Request): ServiceRequest = ServiceRequest(Get, Uri(request.path), request.headerMap.get("Accept").map(AcceptHeader(_)))
  }

  implicit object FromServiceRequestForFinatraRequest extends FromServiceRequest[TFuture, Request] {
    override def apply(serviceRequest: ServiceRequest) = TFuture.value(Request(Method(serviceRequest.method.toString.toUpperCase), serviceRequest.uri.asUriString))
  }


}
