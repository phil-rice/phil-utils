package one.xingyi.core.language

import one.xingyi.core.functions._
import one.xingyi.core.json.{JsonParserLanguage, JsonWriterLangauge}
import one.xingyi.core.monad.{Async, MonadCanFail, MonadWithException}

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}


trait AsyncLanguage {

  def liftTry[M[_], T](tryT: Try[T])(implicit monad: MonadWithException[M]): M[T] = tryT match {
    case Success(t) => monad.liftM(t)
    case Failure(t) => monad.exception(t)
  }


  implicit class AsyncPimper[M[_], T](m: M[T])(implicit async: Async[M]) {
    def await(): T = async.await(m)
  }
  implicit class FailurePimper[Failure](f: Failure) {
    def fail[M[_], T](implicit async: MonadCanFail[M, Failure]): M[T] = async.fail[T](f)
  }

}

object Language extends AnyLanguage with FunctionLanguage with MonadFunctionLanguage with AsyncLanguage with JsonParserLanguage with JsonWriterLangauge
