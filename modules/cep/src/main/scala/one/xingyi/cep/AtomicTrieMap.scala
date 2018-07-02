package one.xingyi.cep
import scala.collection.concurrent.TrieMap
case class AtomicData[V](v: V, id: Long)
import one.xingyi.core.language.FunctionLanguage._

import scala.language.higherKinds
trait AtomicExecutor[M[_]] {
  def apply[K, T](fn: K => T): K => M[T]
}

class AtomicTrieMap[M[_], K, V](default: K => V)(implicit atomicExecutor: AtomicExecutor[M]) {
  def atomically[X](k: K)(fn: K => V => (X, Option[V])): K => M[(X, Option[V])] = atomicExecutor(getFromTrieMap ~+> fn ~^> updateTrieMap(k))

  private val trieMap = TrieMap[K, V]()
  def updateTrieMap[X](k: K): Function[(X, Option[V]), Unit] = {case r@(x, Some(newV)) => trieMap.put(k, newV); case r@(x, _) => trieMap.remove(k)}
  def getFromTrieMap: K => V = { k: K => trieMap.getOrElseUpdate(k, default(k)) }


}
