import $repo.`https://jitpack.io`
import $ivy.`com.github.DistributedClocks:tracechecker:master-SNAPSHOT`

import com.github.distributedclocks.tracechecker._

import java.io.PrintWriter
import java.util.Base64

// traits with that caputre certain fields
sealed trait ClientIdOp {
  val clientId: String
}

sealed trait FailoverOp {
  val serverId: Int
}

sealed trait GIdOp {
  val gId: Long
}

sealed trait Req {
  val clientId: String
  val opId: Long
}

sealed trait Res {
  val opId: Long
  val gId: Long
}

object GIdOrdering extends Ordering[GIdOp] {
  override def compare(x: GIdOp, y: GIdOp): Int = x.gId.compareTo(y.gId)
}

sealed abstract class Record extends Element
// server-related actions
final case class ServerStart(serverId: Int) extends Record
final case class ServerJoining(serverId: Int) extends Record
final case class NextServerJoining(nextServerId: Int) extends Record
final case class NewJoinedSuccessor(nextServerId: Int) extends Record
final case class ServerJoined(serverId: Int) extends Record
final case class ServerFailRecvd(failedServerId: Int) extends Record
final case class NewFailoverSuccessor(newNextServerId: Int) extends Record with FailoverOp {
  override val serverId = newNextServerId
}
final case class NewFailoverPredecessor(newPrevServerId: Int) extends Record with FailoverOp {
  override val serverId = newPrevServerId
}
final case class ServerFailHandled(failServerId: Int) extends Record
final case class PutRecvd(clientId: String, opId: Long, key: String, value: String) extends Record with ClientIdOp
final case class PutOrdered(clientId: String, opId: Long, gId: Long, key: String, value: String) extends Record with ClientIdOp
final case class PutFwd(clientId: String, opId: Long, gId: Long, key: String, value: String) extends Record with ClientIdOp
final case class PutFwdRecvd(clientId: String, opId: Long, gId: Long, key: String, value: String) extends Record with ClientIdOp
final case class PutResult(clientId: String, opId: Long, gId: Long, key: String, value: String) extends Record with ClientIdOp
final case class GetRecvd(clientId: String, opId: Long, key: String) extends Record with ClientIdOp
final case class GetOrdered(clientId: String, opId: Long, gId: Long, key: String) extends Record with ClientIdOp
final case class GetResult(clientId: String, opId: Long, gId: Long, key: String, value: String) extends Record with ClientIdOp

// coord-related actions
final case class CoordStart() extends Record
final case class ServerFail(serverId: Int) extends Record
final case class ServerFailHandledRecvd(failedServerId: Int, adjacentServerId: Int) extends Record
final case class NewChain(chain: String) extends Record
final case class AllServersJoined() extends Record
final case class HeadReqRecvd(clientId: String) extends Record with ClientIdOp
final case class HeadRes(clientId: String, serverId: Int) extends Record with ClientIdOp
final case class TailReqRecvd(clientId: String) extends Record with ClientIdOp
final case class TailRes(clientId: String, serverId: Int) extends Record with ClientIdOp
final case class ServerJoiningRecvd(serverId: Int) extends Record
final case class ServerJoinedRecvd(serverId: Int) extends Record

// kvslib-related actions
final case class KvslibStart(clientId: String) extends Record with ClientIdOp
final case class KvslibStop(clientId: String) extends Record with ClientIdOp
final case class HeadReq(clientId: String) extends Record with ClientIdOp
final case class HeadResRecvd(clientId: String, serverId: Int) extends Record with ClientIdOp
final case class TailReq(clientId: String) extends Record with ClientIdOp
final case class TailResRecvd(clientId: String, serverId: Int) extends Record with ClientIdOp
final case class Put(clientId: String, opId: Long, key: String, value: String) extends Record with ClientIdOp with Req
final case class PutResultRecvd(opId: Long, gId: Long, key: String) extends Record with GIdOp with Res
final case class Get(clientId: String, opId: Long, key: String) extends Record with ClientIdOp with Req
final case class GetResultRecvd(opId: Long, gId: Long, key: String, value: String) extends Record with Res

def decodeChain(chain: String): Array[Byte] =
  Base64.getDecoder.decode(chain)

def chainPp(chain: String): List[Int] =
  decodeChain(chain).map(b => b.toInt).toList

def chainContains(chain: String, serverId: Int): Boolean = chainPp(chain).contains(serverId)

class Spec(N: Int) extends Specification[Record] {
  import Specification._

  val orderedTraces: Query[Map[String,List[Record]]] = {
    materialize {
      call(traces).map { traces =>
        traces.foldLeft(Map.empty: Map[String, List[Record]]){ (m, trace) =>
          m + (trace._1 -> trace._2.sorted(Element.VectorClockOrdering))
        }
      }
    }
  }

  val kvslibStarts: Query[List[KvslibStart]] =
    materialize {
      elements.map(_.collect{ case a: KvslibStart => a })
    }

  val kvslibStops: Query[List[KvslibStop]] =
    materialize {
      elements.map(_.collect{ case a: KvslibStop => a })
    }

  val puts: Query[List[Put]] =
    materialize {
      elements.map(_.collect{ case a: Put => a })
    }

  val gets: Query[List[Get]] =
    materialize {
      elements.map(_.collect{ case a: Get => a })
    }

  val theCoordStart: Query[CoordStart] =
    elements.map(_.collect{ case a: CoordStart => a })
      .requireOne

  // FIXME: should we require only one?
  val allServersJoined: Query[List[AllServersJoined]] =
    elements.map(_.collect{ case a: AllServersJoined => a})

  val serverStart: Query[List[ServerStart]] =
    materialize{ elements.map(_.collect{ case a: ServerStart => a }) }

  val serverJoining: Query[List[ServerJoining]] =
    materialize{ elements.map(_.collect{ case a: ServerJoining => a }) }

  val opsWithClientId: Query[List[Record with ClientIdOp]] =
    materialize{ elements.map(_.collect{ case a: ClientIdOp => a }) }

  val serverJoiningRecvd: Query[List[ServerJoiningRecvd]] =
    materialize{ elements.map(_.collect{ case a: ServerJoiningRecvd => a }) }

  val nextServerJoining: Query[List[NextServerJoining]] =
    materialize{ elements.map(_.collect{ case a: NextServerJoining => a }) }

  val newJoinedSuccessor: Query[List[NewJoinedSuccessor]] =
    materialize{ elements.map(_.collect{ case a: NewJoinedSuccessor => a }) }

  val serverJoined: Query[List[ServerJoined]] =
    materialize{ elements.map(_.collect{ case a: ServerJoined => a }) }

  val serverJoinedRecvd: Query[List[ServerJoinedRecvd]] =
    materialize{ elements.map(_.collect{ case a: ServerJoinedRecvd => a }) }

  val newChain: Query[List[NewChain]] =
    materialize{ elements.map(_.collect{ case a: NewChain => a }) }

  val putRecvd: Query[List[PutRecvd]] =
    materialize{ elements.map(_.collect{ case a: PutRecvd => a }) }

  val getRecvd: Query[List[GetRecvd]] =
    materialize{ elements.map(_.collect{ case a: GetRecvd => a }) }

  val serverFail: Query[List[ServerFail]] =
    materialize{ elements.map(_.collect{ case a: ServerFail => a }) }

  val serverFailRecvd: Query[List[ServerFailRecvd]] =
    materialize{ elements.map(_.collect{ case a: ServerFailRecvd => a }) }

  val failover: Query[List[Record with FailoverOp]] =
    materialize{ elements.map(_.collect{ case a: FailoverOp => a }) }

  val serverFailHandled: Query[List[ServerFailHandled]] =
    materialize{ elements.map(_.collect({ case a: ServerFailHandled => a })) }

  val serverFailHandledRecvd: Query[List[ServerFailHandledRecvd]] =
    materialize{ elements.map(_.collect({ case a: ServerFailHandledRecvd => a })) }

  val headReq: Query[List[HeadReq]] =
    materialize{ elements.map(_.collect({ case a: HeadReq => a })) }

  val headReqRecvd: Query[List[HeadReqRecvd]] =
    materialize{ elements.map(_.collect({ case a: HeadReqRecvd => a })) }

  val headRes: Query[List[HeadRes]] =
    materialize{ elements.map(_.collect({ case a: HeadRes => a })) }

  val headResRecvd: Query[List[HeadResRecvd]] =
    materialize{ elements.map(_.collect({ case a: HeadResRecvd => a })) }

  val tailReq: Query[List[TailReq]] =
    materialize{ elements.map(_.collect({ case a: TailReq => a })) }

  val tailReqRecvd: Query[List[TailReqRecvd]] =
    materialize{ elements.map(_.collect({ case a: TailReqRecvd => a })) }

  val tailRes: Query[List[TailRes]] =
    materialize{ elements.map(_.collect({ case a: TailRes => a })) }

  val tailResRecvd: Query[List[TailResRecvd]] =
    materialize{ elements.map(_.collect({ case a: TailResRecvd => a })) }

  val putResultRecvd: Query[List[PutResultRecvd]] =
    materialize{ elements.map(_.collect({ case a: PutResultRecvd => a })) }

  val getResultRecvd: Query[List[GetResultRecvd]] =
    materialize{ elements.map(_.collect({ case a: GetResultRecvd => a })) }

  def requireTraceType[T](trace: List[Record]): Query[Unit] = {
    val idx = trace.indexWhere(_.isInstanceOf[T])
    if (idx == -1) {
      accept
    } else {
      reject(s"Action ${trace(idx)} is in the wrong trace")
    }
  }

  override def rootRule: RootRule = RootRule(
    multiRule("Initialization", pointValue = 4)(
      rule("KvslibStart exists and happens before KvslibStop/HeadReq/TailReq/Put/Get", pointValue = 1) {
        call(kvslibStarts).quantifying("KvslibStart").forall { kstart =>
          for {
            _ <- call(kvslibStops).quantifying("KvslibStop").forall {
              case kstop if kstart.clientId == kstop.clientId =>
                if (kstart <-< kstop) {
                  accept
                } else {
                  reject("KvslibStart doesn't happen before KvslibStop")
                }
            }
            _ <- call(headReq).quantifying("HeadReq").forall {
              case hreq if kstart.clientId == hreq.clientId =>
                if (kstart <-< hreq) {
                  accept
                } else {
                  reject("KvslibStart doesn't happen before HeadReq")
                }
            }
            _ <- call(tailReq).quantifying("TailReq").forall {
              case treq if kstart.clientId == treq.clientId =>
                if (kstart <-< treq) {
                  accept
                } else {
                  reject("KvslibStart doesn't happen before TailReq")
                }
            }
            _ <- call(puts).quantifying("Put").forall {
              case put if kstart.clientId == put.clientId =>
                if (kstart <-< put) {
                  accept
                } else {
                  reject("KvslibStart doesn't happen before Put")
                }
            }
            _ <- call(gets).quantifying("Get").forall {
              case get if kstart.clientId == get.clientId =>
                if (kstart <-< get) {
                  accept
                } else {
                  reject("KvslibStart doesn't happen before Get")
                }
            }
          } yield ()
        }
      },

      rule("CoordStart recorded exactly once and happens before ServerJoiningRecvd and AllServersJoined", pointValue = 1) {
        for {
          cstart <- call(theCoordStart).label("The CoordStart")
          _ <- call(serverJoiningRecvd).label("ServerJoiningRecvd")
            .require(sjr => s"ServerJoiningRecvd should happen after CoordStart: $sjr") { _.forall(cstart <-< _) }
          _ <- call(allServersJoined).label("AllServerJoined")
            .require(sjr => s"ServerJoiningRecvd should happen after CoordStart: $sjr") { _.forall(cstart <-< _) }
        } yield ()
      },

      rule("Exactly N SeverStart", pointValue = 1) {
        call(serverStart).label("ServerStarts")
          .require(ss => s"There must be exactly N ServerStart actions, $ss") { _.size == N }
      },

      rule("ServerStart happens before ServerJoining", pointValue = 1) {
        call(serverStart).quantifying("ServerStarts").forall { ss =>
          call(serverJoining).quantifying("ServerJoinings").forall { sj =>
            if (ss <-< sj) {
              accept
            } else {
              reject("ServerJoining does not happen after ServerStart")
            }
          }
        }
      }
    ),

    multiRule("Termination", pointValue = 4)(
      rule("KvslibStop cannot followed by any actions by the same client", pointValue = 1) {
        call(kvslibStops).quantifying("KvslibStops").forall { kstop =>
          call(opsWithClientId).quantifying("Actions recorded with the same ClientId as KvslibStop")
            .forall {
              case op if op.clientId == kstop.clientId =>
                if (op <-< kstop) {
                  accept
                } else {
                  reject("Action with the same clientId happens after KvslibStop")
                }
            }
        }
      }
    ),

    multiRule("Join Handling", pointValue = 4)(
      rule("Exactly one ServerJoining for each serverId", pointValue = 1) {
        call(serverJoining)
          .require(sjs => s"No duplicated serverId in ServerJoining actions: $sjs") { sjs =>
            sjs.forall { sj =>
              sjs.count(_.serverId == sj.serverId) == 1
            }
          }
      },
      rule("ServerJoining behaves correctly", pointValue = 1) {
        call(serverJoining).quantifying("ServerJoining").forall{ sj =>
          for {
            _ <- call(serverJoiningRecvd)
              .map(_.collect{ case a if (a.serverId == sj.serverId) && (sj <-< a) => a }).requireOne
              .label("ServerJoiningRecvd")
            nsj <- call(nextServerJoining)
              .map(_.collect{ case a if (a.nextServerId == sj.serverId) && (sj <-< a) => a }).requireAtMostOne
              .label("NextServerJoining")
            _ <- nsj match {
              case Some(next) => if (next.tracerIdentity != sj.tracerIdentity) {
                accept
              } else {
                reject("NextServerJoining is not recorded by a different tracer")
              }
              case _ => accept
            }
            njs <- call(newJoinedSuccessor)
              .map(_.collect{ case a if (a.nextServerId == sj.serverId) && (sj <-< a) => a }).requireAtMostOne
              .label("NewJoinedSuccessor")
            _ <- njs match {
              case Some(next) => if (next.tracerIdentity != sj.tracerIdentity) {
                accept
              } else {
                reject("NewJoinedSuccessor is not recorded by a different tracer")
              }
              case _ => accept
            }
            _ <- call(serverJoined)
              .map(_.collect{ case a if (a.serverId == sj.serverId) && (sj <-< a) => a })
              .label("ServerJoined")
              .requireOne
            _ <- call(serverJoinedRecvd)
              .map(_.collect{ case a if (a.serverId == sj.serverId) && (sj <-< a) => a })
              .label("ServerJoinedRecvd")
              .requireOne
            _ <- call(newChain).quantifying("NewChains").exists {
              case nc if (sj <-< nc) && chainContains(nc.chain, sj.serverId) => accept
            }
          } yield ()
        }
      },
      rule("ServerJoining eventually followed by AllServersJoined", pointValue = 1) {
        call(serverJoining).quantifying("ServerJoinings").forall{ sj =>
          call(allServersJoined).quantifying("AllServersJoined").exists{ allJoined =>
            if (sj <-< allJoined) {
              accept
            } else {
              reject("No AllServersJoined follows ServerJoining")
            }
          }
        }
      },
      rule("AllServersJoined must exist and happen before PutRecvd/GetRecvd", pointValue = 1) {
        call(allServersJoined).requireSome.quantifying("AllServersJoined").exists{ allJoined =>
          for {
            _ <- call(putRecvd).quantifying("any PutRecvd").forall{ pr =>
              if (allJoined <-< pr) accept else reject("AllServersJoined doesn't happen before PutRecvd")
            }
            _ <- call(getRecvd).quantifying("any GetRecvd").forall{ gr =>
              if (allJoined <-< gr) accept else reject("AllServersJoined doesn't happen before GetRecvd")
            }
          } yield ()
        }
      }
    ),

    multiRule("Failure Handling", pointValue = 5)(
      rule("ServerFail followed by one or two ServerFailRecvd", pointValue = 1) {
        call(serverFail).quantifying("all ServerFail").forall { sf =>
          call(serverFailRecvd).map(_.collect{ case a if (sf.serverId == a.failedServerId) && (sf <-< a) => a })
            .require(l => s"ServerFail should only be followed by one or two ServerFailedRecvd, found: $l") { sfr =>
              sfr.size == 1 || sfr.size == 2
            }
        }
      },
      rule("ServerFailRecvd followed by at most one NewFailoverSuccessor or NewFailoverPredecessor", pointValue = 1) {
        call(serverFailRecvd).quantifying("all ServerFailRecvd").forall { sfr =>
          call(failover).map(_.collect{ case a if sfr.failedServerId == a.serverId => a })
            .label("NewFailoverSuccessor or NewFailoverPredecessor")
            .requireAtMostOne
        }
      },
      rule("ServerFailRecvd(S) must be followed by at most one ServerFailHandled(S)", pointValue = 1) {
        call(serverFailRecvd).quantifying("all ServerFailRecvd").forall { sfr =>
          call(serverFailHandled).map(_.collect{ case a if sfr.failedServerId == a.failServerId => a })
            .label("succeeding ServerFailHanlded")
            .requireAtMostOne
        }
      },
      rule("ServerFailHandledRecvd(S) must be preceded by ServerFailHandled(S)", pointValue = 1) {
        call(serverFailHandledRecvd).quantifying("all ServerFailHandledRecvd").forall{ fhr =>
          call(serverFailHandled).map(_.collect{ case a if a <-< fhr => a })
            .label("preceding ServerFailHandled")
            .requireSome
        }
      },
      rule("ServerFail(S) must be eventually followed by NewChain(C) without S", pointValue = 1) {
        call(serverFail).quantifying("all ServerFail").forall { sf =>
          call(newChain).quantifying("exists NewChain").exists {
            case c if sf <-< c && chainContains(c.chain, sf.serverId) => accept
          }
        }
      }
    ),

    multiRule("Join/Failure Handling", pointValue = 1)(
      rule("NewChain must be preceded by either ServerFail or ServerJoined", pointValue = 1) {
        call(newChain).quantifying("all NewChain").forall { nc =>
          call(serverFail).map(_.collect{ case a if a <-< nc => a })
            .flatMap{ sfs =>
              if (sfs.isEmpty) {
                call(serverJoined).map(_.collect{ case a if a <-< nc => a }).requireSome
              } else {
                accept
              }
            }
        }
      }
    ),

    multiRule("Head Server Requests", pointValue = 4)(
      rule("The number of HeadReq(C) and HeadReqRecvd(C) must be identical", pointValue = 1) {
        for {
          hrs <- call(headReq).label("all HeadReq")
          hrrs <- call(headReqRecvd).label("all HeadReqRecvd")
          _ <- if (hrs.size == hrrs.size) accept else reject("Different number of HeadReq and HeadReqRecvd")
        } yield ()
      },
      rule("HeadReq(C) must happen before HeadReqRecvd(C)", pointValue = 1) {
        call(headReq).quantifying("all HeadReq").forall { hreq =>
          for {
            recvd <- call(headReqRecvd).map(_.find(_.clientId == hreq.clientId))
            _ <- recvd match {
              case Some(r) => if (hreq <-< r) accept else reject("HeadReq does not happen before HeadReqRecvd")
              case None => reject("Cannot find the corresponding HeadReqRecvd")
            }
          } yield ()
        }
      },
      rule("The number of HeadRes(C,S) and HeadResRecvd(C,S) must be identical", pointValue = 1) {
        for {
          hrs <- call(headRes).label("all HeadRes")
          hrrs <- call(headResRecvd).label("all HeadResRecvd")
          _ <- if (hrs.size == hrrs.size) accept else reject("Different number of HeadRes and HeadResRecvd")
        } yield ()
      },
      rule("HeadRes(C,S) must happen before HeadResRecvd(C,S)", pointValue = 1) {
        call(headRes).quantifying("all HeadRes").forall { hres =>
          for {
            recvd <- call(headReqRecvd).map(_.find(_.clientId == hres.clientId))
            _ <- recvd match {
              case Some(r) => if (hres <-< r) accept else reject("HeadRes does not happen before HeadResRecvd")
              case None => reject("Cannot find the corresponding HeadResRecvd")
            }
          } yield ()
        }
      }
    ),

    multiRule("Tail Server Requests", pointValue = 4)(
      rule("The number of TailReq(C) and TailReqRecvd(C) must be identical", pointValue = 1) {
        for {
          trs <- call(tailReq).label("all TailReq")
          trrs <- call(tailReqRecvd).label("all TailReqRecvd")
          _ <- if (trs.size == trrs.size) accept else reject("Different number of TailReq and TailReqRecvd")
        } yield ()
      },
      rule("TailReq(C) must happen before TailReqRecvd(C)", pointValue = 1) {
        call(tailReq).quantifying("all TailReq").forall { treq =>
          for {
            recvd <- call(tailReqRecvd).map(_.find(_.clientId == treq.clientId))
            _ <- recvd match {
              case Some(r) => if (treq <-< r) accept else reject("TailReq does not happen before TailReqRecvd")
              case None => reject("Cannot find the corresponding TailReqRecvd")
            }
          } yield ()
        }
      },
      rule("The number of TailRes(C) and TailResRecvd(C) must be identical", pointValue = 1) {
        for {
          trs <- call(tailRes).label("all TailRes")
          trrs <- call(tailResRecvd).label("all TailResRecvd")
          _ <- if (trs.size == trrs.size) accept else reject("Different number of TailRes and TailResRecvd")
        } yield ()
      },
      rule("TailRes(C) must happen before TailResRecvd(C)", pointValue = 1) {
        call(tailRes).quantifying("all TailRes").forall { treq =>
          for {
            recvd <- call(tailResRecvd).map(_.find(_.clientId == treq.clientId))
            _ <- recvd match {
              case Some(r) => if (treq <-< r) accept else reject("TailRes does not happen before TailResRecvd")
              case None => reject("Cannot find the corresponding TailResRecvd")
            }
          } yield ()
        }
      },
    ),

    multiRule("Put Handling", pointValue = 2)(
      rule("Put(C) must be preceded by HeadResRecvd(C,S)", pointValue = 1) {
        call(puts).quantifying("all Puts").forall { p =>
          call(headResRecvd).quantifying("exists headResRecvd").exists { hrr =>
            if (hrr <-< p && hrr.clientId == p.clientId && hrr.tracerIdentity == p.tracerIdentity)
              accept
            else
              reject("No corresponding HeadResRecvd before Put")
          }
        }
      },
      rule("The semantics of Put all recorded in a single Put-Trace", pointValue = 1) {
        call(puts).quantifying("all Puts").forall { p =>
          val ptrace = orderedTraces.map(_.get(p.traceId).toList).requireOne
          for {
            pRecvd <- ptrace.map(_.collect{ case a: PutRecvd => a }).requireSome.map(_.last).label("last PutRecvd")
            pOrdered <- ptrace.map(_.collectFirst{ case a: PutOrdered if (pRecvd <-< a) && (a.tracerIdentity == pRecvd.tracerIdentity) => a }.toList)
              .requireOne
            _ <- ptrace.map(_.collect{ case a: PutFwd if pRecvd <-< a => a })
              .require(_ => "if last PutRecvd is followed by some PutFwd, at least one is recorded at the same server") { pfwds =>
                if (pfwds.nonEmpty) {
                  pfwds.exists(pfwd => pfwd.tracerIdentity == pRecvd.tracerIdentity && pfwd.gId == pOrdered.gId)
                } else {
                  true
                }
              }
            _ <- ptrace.map(_.collect{ case a: PutFwdRecvd if pRecvd <-< a => a })
              .require(_ => "if last PutRecvd is followed by some PutFwdRecvds, then they must recorded by different servers") { pfwdRecvds =>
                if (pfwdRecvds.nonEmpty) {
                  pfwdRecvds.exists { pfwdRecvd =>
                    pfwdRecvd.tracerIdentity != pRecvd.tracerIdentity && pfwdRecvd.gId == pOrdered.gId
                  }
                } else {
                  true
                }
              }
            _ <- ptrace.map(_.collectFirst{ case a: PutResult if a.gId == pOrdered.gId => a }.toList)
              .requireOne
            _ <- ptrace.map(_.collect{ case a: PutResultRecvd if a.gId == pOrdered.gId && a.tracerIdentity == p.tracerIdentity => a })
              .requireOne
          } yield ()
        }
      }
    ),

    multiRule("Get Handling", pointValue = 2)(
      rule("Get(C) must be preceded by TailResRecvd(C,S)", pointValue = 1) {
        call(gets).quantifying("all Gets").forall { g =>
          call(tailResRecvd).quantifying("exists tailResRecvd").exists { trr =>
            if (trr <-< g && trr.clientId == g.clientId && trr.tracerIdentity == g.tracerIdentity)
              accept
            else
              reject("No corresponding TailResRecvd before Get")
          }
        }
      },
      rule("The semantics of Get all recorded in a single Get-Trace", pointValue = 1) {
        call(gets).quantifying("all Gets").forall { g =>
          val gtrace = orderedTraces.map(_.get(g.traceId).toList).requireOne
          for {
            gRecvd <- gtrace.map(_.collect{ case a: GetRecvd => a }).requireSome.map(_.last).label("last GetRecvd")
            gOrdered <- gtrace.map(_.collectFirst{ case a: GetOrdered if (gRecvd <-< a) && (a.tracerIdentity == gRecvd.tracerIdentity) => a }.toList)
              .requireOne
            _ <- gtrace.map(_.collectFirst{ case a: GetResult if a.gId == gOrdered.gId => a }.toList)
              .requireOne
            _ <- gtrace.map(_.collect{ case a: GetResultRecvd if a.gId == gOrdered.gId && a.tracerIdentity == g.tracerIdentity => a })
              .requireOne
          } yield ()
        }
      }
    ),

    multiRule("Put-Get Data Consistency", pointValue = 1)(
      rule("Get must have the same value as its latest preceding Put", pointValue = 1) {
        val putResultRecvdSorted = putResultRecvd.map(_.sorted(GIdOrdering))
        call(puts).quantifying("Put").forall { p =>
          call(putResultRecvdSorted).quantifying("corresponding PutResultRecvd").forall {
            case presRecvd if p.traceId == presRecvd.traceId && p.key == presRecvd.key =>
              call(getResultRecvd).quantifying("corresponding GetResultRecvd").forall {
                case gresRecvd if p.key == gresRecvd.key && presRecvd.gId < gresRecvd.gId =>
                  for {
                    pResultRecvdSorted <- putResultRecvdSorted
                    idx = pResultRecvdSorted.indexOf(presRecvd)
                    nextOpt = pResultRecvdSorted.lift(idx+1)
                    _ = nextOpt match {
                      case Some(next) =>
                        if (gresRecvd.gId < next.gId && gresRecvd.value != p.value)
                          reject("GetResultRecvd doesn't have the same value as its latest preceding Put")
                        else
                          accept
                      case None =>
                        if (gresRecvd.value == p.value)
                          accept
                        else
                          reject("GetResultRecvd doesn't have the same value as its latest preceding Put")
                    }
                  } yield()
              }
          }
        }
      }
    ),

    multiRule("Get Before Any Put Data Consistency", pointValue = 1)(
      rule("Get with no preceding Put should return empty string", pointValue = 1) {
        for {
          earliestPutResRecvdOpt <- putResultRecvd.map(_.sorted(GIdOrdering).headOption)
          _ = call(getResultRecvd).quantifying("GetResultRecvd").forall { gresRecvd =>
            earliestPutResRecvdOpt match {
              case Some(earliestPutResRecvd) if gresRecvd.gId < earliestPutResRecvd.gId && gresRecvd.value != "" =>
                reject("GetResultRecvd with not preceding PutResultRecvd has non-empty value")
              case None if gresRecvd.value != "" =>
                reject("GetResultRecvd with not preceding PutResultRecvd has non-empty value")
              case _ => accept
            }
          }
        } yield ()
      }
    ),

    multiRule("OpID-GID Consistency", pointValue = 1)(
      rule("The OpId for each client should have the same ordering as their corresponding GId", pointValue = 1) {
        import scala.collection.mutable
        val res = for {
          preq <- call(puts).label("Put")
          pres <- call(putResultRecvd).label("PutResultRecvd")
          greq <- call(gets).label("Get")
          gres <- call(getResultRecvd).label("GetResultRecvd")
          res = mutable.Map.empty[String, mutable.ListBuffer[Record with Res]]
          _ = preq.foreach { req =>
            val r = pres.find(x => req.traceId == x.traceId)
            if (r.nonEmpty) {
              if (!res.contains(req.clientId))
                res += req.clientId -> mutable.ListBuffer(r.get)
              res(req.clientId) += r.get
            }
          }
          _ = greq.foreach { req =>
            val r = gres.find(x => req.traceId == x.traceId)
            if (r.nonEmpty) {
              res(req.clientId) += r.get
            }
          }
        } yield res
        res.require(_ => "The OpIds from the same client must respect the gId ordering") { m =>
          m.forall { args =>
            val (_, resRecvds) = args
            resRecvds.combinations(2).forall{ comb =>
              comb.head.opId.compareTo(comb(1).opId) == comb.head.gId.compareTo(comb(1).gId)
            }
          }
        }
      }
    ),
  )
}

// do not remove this. it is here to force Ammonite to read the code ^^^ and the code vvv separately,
// which it turns out is necessary for the @main bit to actually work (and not disappear silently, making this script a no-op)
@

@main
def a3spec(@arg(doc = "the number of servers on the chain") n: Int,
           @arg(doc = "path to the trace file to analyse. this file will the one you told the tracing server to generate, and should contain exactly one trace") traceFiles: os.Path*): Unit = {
  val spec = new Spec(n)
  if (traceFiles.isEmpty) {
    println("No trace file provided.")
    sys.exit(0)
  }
  val results = spec.checkRules(traceFiles:_*)
  if (results.success) {
    println("all checks passed!")
    println()
    println("summary:")
    results.ruleList().foreach(print)
  } else {
    println(s"some checks failed... approximate grade: ${results.grade}/${spec.rootRule.availablePts}")
    println()
    println("summary:")
    results.ruleList().foreach(print)
    println()
    println("details:")
    results.counterExamples().foreach(print)
  }
//  println("Score: " + results.grade)
//  results.dump().foreach(print)
//  val p = new PrintWriter("grade_out.log")
//  try {
//    p.println(results.grade)
//    results.dump().foreach(p.print)
//  } finally {p.close()}
}
