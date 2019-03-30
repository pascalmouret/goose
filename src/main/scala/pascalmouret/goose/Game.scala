package pascalmouret.goose

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

case class PlayerExistsException(name: String) extends Exception(s"Player already exists: $name")
case class PlayerDoesntExistsException(name: String) extends Exception(s"Player doesn't exists: $name")

/**
  * A representation of a board the game can be played on.
  *
  * The [[Game]] assumes the board is "sane" and will not do any checks. This specifically means:
  *  - [[lastSpace]] must not be smaller than 12
  *  - bridges and goose spaces must not contain infinite loops
  *  - bridges must not lead outside the board
  *
  * @param lastSpace How many fields the board has (excluding start)
  * @param gooseSpaces Fields that will double movement if the players lands on them
  * @param bridges Connect two points on the board. Are one way only (source -> destination).
  */
case class Board(
  lastSpace: Int,
  gooseSpaces: Set[Int],
  bridges: Map[Int, Int]
)

case class Player(
  name: String,
  position: Int = 0
)

sealed trait Event
case class PlayerLandedOn(name: String, space: Int) extends Event
case class PlayerLandedOnGoose(name: String, space: Int) extends Event
case class PlayerTraversedBridge(name: String, from: Int, to: Int) extends Event
case class PlayerBounced(name: String) extends Event
case class PlayerWon(name: String) extends Event
case class PlayerAdded(name: String) extends Event
case class PlayerRolledDice(name: String, total: Int) extends Event
case class PlayerGotPranked(victim: String, prankster: String, destination: Int) extends Event

/**
  * A representation of a given game state. Any manipulation of the state will yield a new instance of [[Game]].
  *
  * A running game will be an instance of [[RunningGame]]. If a player wins the game, an instance of [[FinishedGame]]
  * will be returned and any manipulation will no longer change the game state.
  */
sealed trait Game {
  val board: Board
  val players: Set[Player]
  val log: List[Event]

  def playerPosition(name: String): Int = {
    players.find(_.name == name).map(_.position).getOrElse(throw PlayerDoesntExistsException(name))
  }

  def addPlayer(name: String): Game = {
    if (players.exists(_.name == name)) {
      throw PlayerExistsException(name)
    } else {
      nextState(players = players + Player(name), List(PlayerAdded(name)))
    }
  }

  def movePlayer(name: String): Game = {
    val rand = scala.util.Random
    movePlayer(name, rand.nextInt(5) + rand.nextInt(5) + 2)
  }

  def movePlayer(name: String, by: Int): Game = {
    players.find(_.name == name).map(movePlayer(_, by, List(PlayerRolledDice(name, by))))
      .getOrElse(throw PlayerDoesntExistsException(name))
  }

  /**
    * Here the next position is determined based on current position and the total dice roll. Because it's recursive
    * it can technically solve any combinations of rules. That means it could also end up in an infinite loop on a
    * badly designed board.
    *
    * Note that the recursive calls do some weird math by subtracting the "by" from the next position. This is done
    * so the "by" parameter can be passed on for use by the goose fields.
    *
    * @param player The player to be moved.
    * @param by How many spaces the player will move.
    * @param log Log of events that already occurred.
    * @return The game state after the player was moved.
    */
  @tailrec
  private def movePlayer(player: Player, by: Int, log: List[Event] = List.empty): Game = {
    player.position + by match {
      case sum if sum > board.lastSpace =>
        // this could lead to an undefined state (negative position) if the field is smaller than the maximal dice roll
        movePlayer(
          player.copy(position = board.lastSpace -(sum - board.lastSpace) - by),
          by,
          log :+ PlayerBounced(player.name)
        )
      case sum if board.gooseSpaces.contains(sum) =>
        movePlayer(player.copy(position = sum), by, log :+ PlayerLandedOnGoose(player.name, sum))
      case Bridge(from, to) =>
        movePlayer(player.copy(position = to - by), by, log :+ PlayerTraversedBridge(player.name, from, to))
      case sum =>
        // we map over the current players to adjust their positions, including pranks
        val newLogs = ListBuffer.empty[Event] // mutable to simplify the code
        val newPlayers = players map {
          case p @ Player(name, pos) if pos == sum && name != player.name =>
            val destination = players.find(_.name == player.name).get.position
            newLogs.append(PlayerGotPranked(p.name, player.name, destination))
            p.copy(position = destination)
          case Player(name, _) if name == player.name =>
            newLogs.prepend(PlayerLandedOn(player.name, sum))
            player.copy(position = sum)
          case p => p
        }
        nextState(newPlayers, log ++ newLogs.toList)
    }
  }

  /**
    * Helper to make pattern matching nicer.
    */
  object Bridge {
    def unapply(arg: Int): Option[(Int, Int)] = {
      board.bridges.get(arg).map(arg -> _)
    }
  }

  protected def nextState(players: Set[Player], log: List[Event]): Game
}

case class RunningGame(board: Board, players: Set[Player], log: List[Event]) extends Game {
  protected def nextState(players: Set[Player], log: List[Event]): Game = {
    players.find(_.position == board.lastSpace)
      .map(p => FinishedGame(board, players, p, log :+ PlayerWon(p.name)))
      .getOrElse(RunningGame(board, players, log))
  }
}
case class FinishedGame(board: Board, players: Set[Player], winner: Player, log: List[Event]) extends Game {
  override def nextState(players: Set[Player], log: List[Event]): Game = this
}

object Game {
  def apply(players: Set[Player], board: Board): RunningGame = RunningGame(board, players, List.empty)
  def apply(board: Board): RunningGame = RunningGame(board, Set.empty, List.empty)
}
