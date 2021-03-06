package one.xingyi.core.monad

import java.util.concurrent.atomic.AtomicInteger

import scala.language.higherKinds

trait Liftable[M[_]]{
  def liftM[T](t: T): M[T]

}
trait Functor[M[_]] extends Liftable[M]{
  def map[T, T1](m: M[T], fn: T => T1): M[T1]
}

trait Monad[M[_]] extends Functor[M] {
  def flatMap[T, T1](m: M[T], fn: T => M[T1]): M[T1]
  def flattenM[T](ms: Seq[M[T]]): M[Seq[T]] = ms.foldLeft(liftM(Seq[T]())) { (mAcc: M[Seq[T]], mV: M[T]) => flatMap(mAcc, (acc: Seq[T]) => flatMap(mV, (v: T) => liftM(acc :+ v))) }
}

trait MonadWithException[M[_]] extends Monad[M] {
  def exception[T](t: Throwable): M[T]
  def recover[T](m: M[T], fn: Throwable => M[T]): M[T]
}

trait LiftFailure[M[_], Fail] {
  def fail[T](f: Fail): M[T]
}

trait MonadCanFail[M[_], Fail] extends Monad[M] with LiftFailure[M, Fail] {
  def flatMapEither[T, T1](m: M[T], fn: Either[Fail, T] => M[T1]): M[T1]
}

object MonadCanFail {
  implicit def monadCanFailForEither[Fail]: MonadCanFail[({type λ[α] = Either[Fail, α]})#λ, Fail] = new MonadCanFailForEither[Fail]
}

trait MonadCanFailWithException[M[_], Fail] extends MonadWithException[M] with MonadCanFail[M, Fail] {
  def foldWithExceptionAndFail[T, T1](m: M[T], fnE: Throwable => M[T1], fnFailure: Fail => M[T1], fn: T => M[T1]): M[T1]
}

class MonadCanFailForEither[Fail] extends MonadCanFail[({type λ[α] = Either[Fail, α]})#λ, Fail] {
  type M[T] = Either[Fail, T]
  override def fail[T](f: Fail): Either[Fail, T] = Left(f)
  override def liftM[T](t: T): Either[Fail, T] = Right(t)
  override def flatMap[T, T1](m: Either[Fail, T], fn: T => M[T1]): M[T1] = m.right.flatMap(fn)
  override def map[T, T1](m: Either[Fail, T], fn: T => T1): M[T1] = m.right.map(fn)
  override def flatMapEither[T, T1](m: Either[Fail, T], fn: Either[Fail, T] => M[T1]): Either[Fail, T1] = fn(m)
}


trait MonadWithState[M[_]] extends Monad[M] {
  def mapState[V, T, T1](m: M[T], localVariable: LocalVariable[V], fn: Seq[V] => T1): M[T1]
  def mapWith[V, T, T1](m: M[T], localVariable: LocalVariable[V], fn: (T, Seq[V]) => T1): M[T1]
  def putInto[V, T](localVariable: LocalVariable[V], t: V)(m: M[T]): M[T]
  def liftMAndPut[T, V](t: T, localVariable: LocalVariable[V], v: V): M[T] = putInto(localVariable, v)(liftM(t))
}

object LocalVariable {
  private val nextIndex = new AtomicInteger()
  def apply[V]() = new LocalVariable[V](nextIndex.getAndIncrement())
}

class LocalVariable[V](val offset: Int) {
  //this is safe, and is the price for not having horrible type signatures polluting every method
  def getFrom(state: Map[Int, Seq[Any]]): Seq[V] = state.getOrElse(offset, Nil).asInstanceOf[Seq[V]]
}
