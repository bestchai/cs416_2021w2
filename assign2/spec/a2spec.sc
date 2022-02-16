import $repo.`https://jitpack.io`
import $ivy.`com.github.DistributedClocks:tracechecker:master-SNAPSHOT`

import com.github.distributedclocks.tracechecker._

import java.io.PrintWriter
import java.util.Base64

sealed trait StateMoveMessage {
  val gameState: Option[String]
  val moveRow: Int
  val moveCount: Int
  val tracingServerAddr: String
  // FIXME: the type of token may need to change
  val token: Option[String]
}

def getGameStateBytes(gameState: String): Array[Byte] =
  Base64.getDecoder.decode(gameState)

def gameStatePp(gameState: String): List[Int] =
  getGameStateBytes(gameState).map(b => b.toInt).toList

object StateMoveMessage {
  def unapply(candidate: Any): Option[(Option[String],Int,Int)] =
    candidate match {
      case stateMoveMessage: StateMoveMessage =>
        Some((stateMoveMessage.gameState, stateMoveMessage.moveRow, stateMoveMessage.moveCount))
      case _ => None
    }
}

sealed abstract class Record extends Element {
  override def toString: String = {
    s"[$lineNumber] $productPrefix(${
      (productElementNames zip productIterator)
        .map {
          case name -> (value: Option[String]) if name == "gameState" =>
            val gameState =
              if (value.isDefined) {
                gameStatePp(value.get)
              } else {
                value.toString
              }
            s"$name = $gameState"
          case name -> value => s"$name = $value"
        }
        .mkString(", ")
    })@$tracerIdentity{${
      vectorClock
        .keysIterator
        .toArray
        .sortInPlace()
        .iterator
        .map(key => s"$key -> ${vectorClock(key)}")
        .mkString(", ")
    }}#$traceId"
  }
}
final case class GameStart(seed: Int) extends Record
final case class ClientMove(gameState: Option[String], moveRow: Int, moveCount: Int, tracingServerAddr: String, token: Option[String]) extends Record with StateMoveMessage
final case class ServerMoveReceive(gameState: Option[String], moveRow: Int, moveCount: Int, tracingServerAddr: String, token: Option[String]) extends Record with StateMoveMessage
final case class GameComplete(winner: String) extends Record

// new actions in A2
final case class NewNimServer(nimServerAddress: String) extends Record
final case class NimServerFailed(nimServerAddress: String) extends Record
final case class AllNimServersDown() extends Record

// server-side actions
final case class ServerGameStart(gameState: Option[String], moveRow: Int, moveCount: Int, tracingServerAddr: String, token: Option[String]) extends Record with StateMoveMessage
final case class ServerMove(gameState: Option[String], moveRow: Int, moveCount: Int, tracingServerAddr: String, token: Option[String]) extends Record with StateMoveMessage
final case class ClientMoveReceive(gameState: Option[String], moveRow: Int, moveCount: Int, tracingServerAddr: String, token: Option[String]) extends Record with StateMoveMessage
final case class GameResume(gameState: Option[String], moveRow: Int, moveCount: Int, tracingServerAddr: String, token: Option[String]) extends Record with StateMoveMessage
final case class ServerFailed(serverAddress: String) extends Record

class Spec(seed: String, N: Int) extends Specification[Record] {
  import Specification._

  val theTrace: Query[List[Record]] =
    traces.requireOne.map(_._2)

  val theTraceInOrder: Query[List[Record]] =
    materialize {
      // in this specific assignment, the vector clocks should form a total order, which we can use to sort them
      call(theTrace).map(_.sorted(Element.VectorClockOrdering))
        .flatMap { trace =>
          // sanity check: under some weird conditions where the tracing library is not used properly (branching),
          // the statement about total order above may not actually be true.
          // let's just check this to be sure, rather than give confusing feedback based on ambiguous information,
          // and hopefully no-one will ever fail this condition
          accept(trace zip trace.tail).quantifying("sequential pair").forall {
            case (before, after) if before <-< after => accept
            case (before, after) =>
              for {
                _ <- label("before")(before)
                _ <- label("after")(after)
                _ <- reject("before should happen-before after, but doesn't. your vector clocks are probably corrupted")
              } yield ()
          }
            .map(_ => trace)
        }
    }

  // A2 helper functions
  val theServerGameStart: Query[ServerGameStart] =
    call(theTrace)
      .map(_.collect { case sgs: ServerGameStart => sgs })
      .requireOne

  val ifTheServerGameStart: Query[Option[ServerGameStart]] =
    call(theTrace)
      .map(_.collect { case sgs: ServerGameStart => sgs })
      .requireAtMostOne

  val ifGameComplete: Query[Option[GameComplete]] =
    call(theTrace)
      .map(_.collect { case gc: GameComplete => gc })
      .requireAtMostOne

  val ifAllNimServersDown: Query[Option[AllNimServersDown]] =
    call(theTrace)
      .map(_.collect { case allDown: AllNimServersDown => allDown })
      .requireAtMostOne

  // A total ordered trace where incomparable VCs are treated as equal
  val theTotalOrderedTrace: Query[List[Record]] =
    materialize {
      call(theTrace).map(_.sorted(Element.VectorClockOrdering))
    }

  val clientMoveReceive: Query[List[ClientMoveReceive]] =
    call(theTrace)
      .map(_.collect { case cmr: ClientMoveReceive => cmr })

  val serverMoveReceive: Query[List[ServerMoveReceive]] =
    call(theTrace)
      .map(_.collect { case sm: ServerMoveReceive => sm })

  val newNimServer: Query[List[NewNimServer]] = {
    materialize {
      call(theTrace)
        .map(_.collect { case nns: NewNimServer => nns })
    }
  }

  val nimServerFailed: Query[List[NimServerFailed]] = {
    materialize {
      call(theTrace)
        .map(_.collect { case nsf: NimServerFailed => nsf })
    }
  }

  val serverFailed: Query[List[ServerFailed]] = {
    materialize {
      call(theTrace)
        .map(_.collect { case sf: ServerFailed => sf })
    }
  }

  val gameResume: Query[List[GameResume]] = {
    materialize {
      call(theTrace)
        .map(_.collect { case gr: GameResume => gr })
    }
  }

  val gameStart: Query[List[GameStart]] =
    call(theTrace)
      .map(_.collect { case gs: GameStart => gs })

  val duplicatedMsgs: Query[Set[ById[ServerMoveReceive]]] =
    materialize {
      call(theTotalOrderedTrace)
        .map(_.collect {
          case m: ServerMoveReceive => m
        })
        .map(_.foldLeft((Set.empty[ServerMoveReceive], Set.empty[ById[ServerMoveReceive]])){ (tuple, item) =>
          val (seen, dups) = tuple
          val newdup =
            if (seen(item)) {
              dups.incl(ById(item))
            } else {
              dups
            }
          (seen.incl(item), newdup)
        })
        .map(_._2)
    }

  def requireLegalOnReceive(m: StateMoveMessage): Query[Unit] =
    m match {
      case ClientMove(None, -1, s, _, _) if s.toString == seed =>
        accept // always legal on receive. we will try to figure out game begin semantics elsewhere
      case sm: ServerMoveReceive =>
        // we check that ServerMove happened in response to at least _something_ the client sent.
        // while a lot of things can happen due to retries/delays/etc, you can never have a server response without at
        // least one client request.
        causalRelation.latestPredecessors(sm) { case cm: ClientMove => cm }
          .label("latest predecessors")
          .requireOne
          .asUnit
      case cm@ClientMove(Some(gameStateAfterStr), moveRow, moveCount, _, _) if moveRow >= 0 && moveCount >= 0 =>
        // for non-initial client moves, we look back to the "last" ServerMove the client reported seeing to determine
        // if the client's request was reasonable. this avoids all the messy UDP semantics, as well as second-guessing
        // what the server might be seeing (which may include UDP-induced invalid moves!)
        duplicatedMsgs.flatMap { duplicatedMsgs =>
          causalRelation.latestPredecessors(cm) { case sm@ServerMoveReceive(Some(_), _, _, _, _) if !duplicatedMsgs(ById(sm)) => sm }
            .label("latest predecessors")
            .requireOne
            .flatMap {
              case sm@ServerMoveReceive(Some(gameStateBeforeStr), _, _, _, _) =>
                val gameStateAfter = getGameStateBytes(gameStateAfterStr)
                val gameStateBefore = getGameStateBytes(gameStateBeforeStr)
                for {
                  _ <- label("gameStateBefore")(gameStateBefore)
                  _ <- label("gameStateAfter")(gameStateAfter)
                  _ <- if(moveCount == 0) {
                    reject(s"$cm has a move count of 0, which Nim does not allow")
                  } else if(!gameStateBefore.indices.contains(moveRow)) {
                    reject(s"$cm lists a moveRow that does not index into the board in $sm")
                  } else {
                    val nextRowVal = gameStateBefore(moveRow) - moveCount
                    if (nextRowVal < 0) {
                      reject(s"$cm implies a game board with a negative value, relative to $sm")
                    } else {
                      if(gameStateAfter.corresponds(gameStateBefore.view.updated(moveRow, nextRowVal))(_ == _)) {
                        accept
                      } else {
                        reject(s"the game board in $cm is not consistent with the one in $sm, according to Nim rules")
                      }
                    }
                  }
                } yield ()
            }
            .asUnit
        }
      case _ =>
        reject(s"the move did not fit any recognised pattern. maybe it's a checker bug or a corrupt trace?")
    }

  override def rootRule: RootRule = RootRule(
    multiRule("[25%] Distributed tracing works", pointValue = 25)(
      rule("[5%] ServerGameStart is recorded after the first ClientMove", pointValue = 5) {
        call(ifTheServerGameStart).quantifying("the ServerGameStart").forall { sgs =>
          call(theTotalOrderedTrace).map(_.collectFirst { case cm : ClientMove => cm })
            .map(_.toList)
            .requireOne
            .flatMap { cm =>
              require("The first ClientMove happens-before ServerGameStart") {
                cm <-< sgs
              }
            }
        }
      },
      rule("[10%] A ClientMove is recorded before each ClientMoveReceive", pointValue = 10){
        call(clientMoveReceive)
          .map(_.sorted(Element.VectorClockOrdering))
          .quantifying("ClientMoveReceive").forall { cmr =>
            causalRelation.latestPredecessors(cmr) { case cm : ClientMove => cm }
              .label("the earliest successor of the ClientMove")
              .require(cm => s"the ClientMove $cm should match ClientMoveReceive $cmr")(_.exists { cm =>
                cmr.moveRow == cm.moveRow && cmr.moveCount == cmr.moveCount && cmr.gameState == cmr.gameState
              })
          }
      },
      rule("[10%] A ServerMoves is recorded before each ServerMoveReceive", pointValue = 10){
        call(serverMoveReceive).quantifying("server move").forall { smr =>
            causalRelation.latestPredecessors(smr) { case sm : ServerMove => sm }
              .label("the latest predecessor of the ServerMoveReceive")
              .require(sm => s"the ServerMoveReceive $smr should match ServerMove $sm") (_.exists { sm =>
                smr.moveRow == sm.moveRow && smr.moveCount == sm.moveCount && smr.gameState == sm.gameState
              })
          }
      }
    ),

    multiRule("[25%] Nim server failures are detected by fcheck", pointValue = 25)(
      rule("[10%] If NimServerFailed is recorded, then there is a NewNimServer happens before it with the identical address", pointValue = 10){
        call(nimServerFailed).quantifying("NimServerFailed").forall { fail =>
          call(newNimServer).quantifying("NewNimServer").exists { newServer =>
            if (newServer.nimServerAddress == fail.nimServerAddress && newServer <-< fail) {
              accept
            } else {
              reject("There must exist a corresponding NewNimServer for every NimServerFailed")
            }
          }
        }
      },
      rule("[15%] NimServerFailed is recorded when there is a corresponding ServerFailed", pointValue = 15){
        call(nimServerFailed).quantifying("NimServerFailed").forall { fail =>
          call(serverFailed).quantifying("ServerFailed").exists { sf =>
            val port1 = sf.serverAddress.split(':')(1)
            val port2 = fail.nimServerAddress.split(':')(1)
            if (port1 == port2) {
              accept
            } else {
              reject("There must exist a corresponding ServerFailed for every NimServerFailed")
            }
          }
        }
      }
    ),

    multiRule("[40%] Transparent nim server fail-over works", pointValue = 40)(
      rule("[10%] When there is a GameComplete, NewNimServer is recorded after each NimServerFailed", pointValue = 10) {
        call(ifGameComplete).quantifying("GameComplete").forall { _ =>
          call(nimServerFailed).quantifying("NimServerFailed").forall { fail =>
            call(newNimServer).quantifying("NewNimServer").exists { newServer =>
              if (fail <-< newServer) {
                accept
              } else {
                reject("There must be a subsequent NewNimServer after each NimServerFailed")
              }
            }
          }
        }
      },
      rule("[15%] When there is a GameComplete, ServerGameStart or GameResume is recorded after NimServerFailed (i.e., NimServerFailed happens-before ServerGameStart/GameResume)", pointValue = 15) {
        call(ifGameComplete).quantifying("GameComplete").forall { _ =>
          var gameStarted = false
          call(nimServerFailed)
            .map(_.sorted(Element.VectorClockOrdering))
            .quantifying("NimServerFailed").forall { fail =>
            if (!gameStarted) {
              for {
                sgs <- call(theServerGameStart).label("the ServerGameStart")
                grs <- call(gameResume).label("GameResumes")
                _ <- if (fail <-< sgs) {
                  accept
                } else if (grs.exists(gr => fail <-< gr)) {
                  gameStarted = true
                  accept
                } else {
                  reject("The game must start or resume after the first NimServerFailed")
                }
              } yield ()
            } else {
              call(gameResume).quantifying("GameResume").exists { resume =>
                if (fail <-< resume) {
                  accept
                } else {
                  reject("The game must resume after NimServerFailed")
                }
              }
            }
          }
        }
      },
      rule("[15%] When there is a GameComplete, the game progress normally, like A1", pointValue = 15) {
        call(ifGameComplete).quantifying("GameComplete").forall { gc =>
          for {
            _ <- call(theTrace).quantifying("move").forall {
              case m: ClientMove => call(requireLegalOnReceive(m))
              case m: ServerMoveReceive => call(requireLegalOnReceive(m))
            }
            _ <- causalRelation.latestPredecessors(gc) {
              case cm: ClientMove => cm
              case smr: ServerMoveReceive => smr
            }
              .requireOne
              .flatMap {
                case StateMoveMessage(Some(boardStr), _, _) if getGameStateBytes(boardStr).forall(_ == 0) => accept
                case m => reject(s"the last move did not contain a board with all 0s, $m")
              }
          } yield ()
        }
      }
    ),

    multiRule("[10%] Nim servers total failure handled properly", pointValue = 10)(
      rule("[5%] If AllNimServersDown is recorded, it only appear once and GameComplete does not exist", pointValue = 5) {
        call(ifAllNimServersDown).quantifying("AllNimServersDown").forall { _ =>
          call(ifGameComplete).quantifying("GameComplete").forall { _ =>
            reject("GameComplete must not co-exist with AllNimServersDown")
          }
        }
      },
      rule("[5%] If AllNimServersDown is recorded, N NimServerFailed between the last ServerMoveReceive and AllNimServersDown", pointValue = 5){
        call(ifAllNimServersDown).quantifying("AllNimServersDown").forall { _ =>
          call(theTotalOrderedTrace).map { trace =>
            val idx = trace.lastIndexWhere( rc => rc.isInstanceOf[ServerMoveReceive] )
            if (idx == -1) {
              trace
            } else {
              trace.splitAt(idx)._2
            }
          }.require(trace => s"The (sub)trace must have exactly N NimServerFailed actions.\n(sub)trace: $trace") { trace =>
            val c = trace.count {
              case _: NimServerFailed => true
              case _ => false
            }
            c == N
          }
        }
      }
    )
  )
}

// do not remove this. it is here to force Ammonite to read the code ^^^ and the code vvv separately,
// which it turns out is necessary for the @main bit to actually work (and not disappear silently, making this script a no-op)
@

@main
def a2spec(@arg(doc = "the seed passed to the client which produced the trace being checked, in decimal with no leading zeroes") seed: String,
           @arg(doc = "the seed passed to the client which produced the trace being checked, in decimal with no leading zeroes") n: Int,
           @arg(doc = "path to the trace file to analyse. this file will the one you told the tracing server to generate, and should contain exactly one trace") traceFiles: os.Path*): Unit = {
  val spec = new Spec(seed = seed, N = n)
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
}
