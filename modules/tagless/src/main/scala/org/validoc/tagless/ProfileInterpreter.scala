package org.validoc.tagless

import org.validoc.utils.aggregate.{Enricher, HasChildren}
import org.validoc.utils.cache.{CachableKey, ShouldCacheResult, ShouldUseCache}
import org.validoc.utils.endpoint.MatchesServiceRequest
import org.validoc.utils.functions.{Liftable, Monad, MonadWithException}
import org.validoc.utils.http._
import org.validoc.utils.logging.{DetailedLogging, SummaryLogging}
import org.validoc.utils.metrics.ReportData
import org.validoc.utils.profiling.{ProfileAs, ProfileKleisli, TryProfileData}
import org.validoc.utils.retry.{NeedsRetry, RetryConfig}
import org.validoc.utils.strings.{IndentAnd, Strings}
import org.validoc.utils.time.NanoTimeService

import scala.collection.concurrent.TrieMap
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.language.implicitConversions

trait Profiled {
  def name: String
  def description: String
  def tryProfileData: TryProfileData
  def allChildren: Seq[Profiled]
}

trait TaglessProfiles[M[_]] extends TaglessRoot[M] {

  //  type K[Req, Res] = Req => M[Res]

  //TODO Need to add children to this so that we can do an HTML dump of it.
  // But this looks like it will work!
  //So this wraps every node in out tree, and we should be able to see the web page with the profiles of everything!
  case class ProfilingWrapper[Req: ClassTag, Res: ClassTag](name: String, description: String, kleisli: Req => M[Res], children: ProfilingWrapper[_, _]*)(implicit monadWithException: MonadWithException[M], nanoTimeService: NanoTimeService) extends PartialFunction[Req, M[Res]] with K[Req, Res] with Profiled {
    val tryProfileData = new TryProfileData
    val profiledKleisli = ProfileKleisli(tryProfileData)(kleisli)

    override def apply(v1: Req) = profiledKleisli(v1)
    val allChildren: Seq[Profiled] = children.flatMap(child => Seq(child) ++ child.allChildren)
    def indents[T](fn: ProfilingWrapper[_, _] => T): IndentAnd[T] = {
      IndentAnd.merge(fn(this), children.map(_.indents(fn)): _*)
    }

    import org.validoc.utils.reflection.ClassTags._

    override def toString = s"ProfilingWrapper  ${children.size} ${allChildren.size} ($name, $description) [${nameOf[Req]}, ${nameOf[Res]}]  }"
    override def isDefinedAt(x: Req) = kleisli match {
      case pf: PartialFunction[Req, _] => pf.isDefinedAt(x)
      case _ => true
    }
  }

  import org.validoc.utils.language.Language._

  def endpointForProfiler[Req, Res](name: String, interpreter: TaglessLanguage[ProfilingWrapper, M], profilingWrapper: ProfilingWrapper[Req, Res])(implicit monadWithException: MonadWithException[M]): ProfilingWrapper[ServiceRequest, Option[ServiceResponse]] =
    interpreter.endpoint[ServiceRequest, ServiceResponse](name, MatchesServiceRequest.fixedPath(Get))(
      ProfilingWrapper("endpointForProfiler", name, { serviceRequest: ServiceRequest =>
        val indentAndString = profilingWrapper.indents(pf => (s"${pf.name} ${pf.description}", pf.tryProfileData.toShortString))

        val result = "<table><tr>" + indentAndString.invertIndent.toString("</tr><tr>", { case (depth, (l, r)) => s"<td>${Strings.indent("&nbsp;&nbsp;&nbsp;", depth)}$l</td><td>$r</td>" }) + "</tr></table>"
        ServiceResponse(result).liftM[M]
      }))

  def profileEachEndpoint(profileEndpoint: String, interpreter: TaglessLanguage[K, M])(implicit monadWithException: MonadWithException[M]) = new TransformTaglessLanguage[K, ProfilingWrapper, M](new WrapperTransformer[K, ProfilingWrapper, M] {
    override def apply[Req: ClassTag, Res: ClassTag](name: String, description: String, fn: TaglessLanguage[K, M] => K[Req, Res], children: ProfilingWrapper[_, _]*): ProfilingWrapper[Req, Res] =
      ProfilingWrapper(name, description, fn(interpreter), children: _*)

  }) {
    override def extraEndpoints(wrapper: ProfilingWrapper[ServiceRequest, Option[ServiceResponse]]): ProfilingWrapper[ServiceRequest, Option[ServiceResponse]] =
      endpointForProfiler(profileEndpoint, this, wrapper)
  }

  def makeSystemAndProfileEndpoint[X](interpreter: TaglessLanguage[K, M],
                                      name: String,
                                      maker: TaglessLanguage[ProfilingWrapper, M] => X,
                                      endPoints: X => ProfilingWrapper[ServiceRequest, Option[ServiceResponse]])(implicit monadWithException: MonadWithException[M]): (X, ProfilingWrapper[ServiceRequest, Option[ServiceResponse]]) = {
    val profiledAllLanguage: TaglessLanguage[ProfilingWrapper, M] = profileEachEndpoint(name, interpreter)
    val result: X = maker(profiledAllLanguage)
    (result, endpointForProfiler(name, profiledAllLanguage, endPoints(result)))
  }

}

abstract class WrapperTransformer[Wrapper[_, _], Wrapper2[_, _], M[_]](implicit evidence: Wrapper2[_, _] <:< Wrapper[_, _]) {
  def apply[Req: ClassTag, Res: ClassTag](name: String, description: String, fn: TaglessLanguage[Wrapper, M] => Wrapper[Req, Res], children: Wrapper2[_, _]*): Wrapper2[Req, Res]

}

import org.validoc.utils.reflection.ClassTags._


class ProfileEachEndpointLanguage[Wrapper[_, _], M[_] : Monad](interpreter: TaglessLanguage[Wrapper, M]) extends DelegatesTaglessLanguage[Wrapper, M](interpreter) {
  //TODO So here we have an interesting example of where a State monad would actually help. I think it would mean we didn't have mutable code and the interpreter wasn't stateless
  //This code is remarkably easier though...
  val map = TrieMap[String, TryProfileData]()
  override def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: Wrapper[Req, Res])(implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]) = {
    val data = map.getOrElseUpdate(normalisedPath + matchesServiceRequest, new TryProfileData)
    interpreter.endpoint(normalisedPath, matchesServiceRequest)(raw |+| profile(data))
  }
  override def extraEndpoints(wrapper: Wrapper[ServiceRequest, Option[ServiceResponse]]): Wrapper[ServiceRequest, Option[ServiceResponse]] =
    profileMetricsEndpoint("/profiles")


  def dump = ServiceResponse(Status(200), Body(map.map {
    case (k, v) => (f"$k%-40s" + " " + v.toShortString).replaceAll(" ", "&nbsp;")
  }.mkString("<br />")), ContentType("text/html "))

  def profileMetricsEndpoint(profileEndpoint: String) = function[ServiceRequest, ServiceResponse]("profileMetricsEndpoint")(_ => dump) |+| endpoint[ServiceRequest, ServiceResponse](profileEndpoint, MatchesServiceRequest.fixedPath(Get))

}

