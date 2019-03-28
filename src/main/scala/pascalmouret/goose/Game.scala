package pascalmouret.goose

case class PlayerExistsException(name: String) extends Exception(s"Player already exists: $name")
case class PlayerDoesntExistsException(name: String) extends Exception(s"Player doesn't exists: $name")

/**
  * A representation of a given game state. Any manipulation of the state will yield a new instance of [[Game]].
  *
  * A running game will be an instance of [[RunningGame]]. If a player wins the game, an instance of [[FinishedGame]]
  * will be returned and any manipulation will no longer change the game state.
  */
sealed trait Game {
  val board: Board
  val players: Set[Player]

  def playerPosition(name: String): Int = {
    players.find(_.name == name).map(_.position).getOrElse(throw PlayerDoesntExistsException(name))
  }

  def addPlayer(name: String): Game = {
    if (players.exists(_.name == name)) {
      throw PlayerExistsException(name)
    } else {
      nextState(players = players + Player(name))
    }
  }

  def movePlayer(name: String, by: Int): Game = {
    if (players.exists(_.name == name)) {
      nextState(players = players map {
        case p @ Player(n, pos) if n == name =>
          p.copy(position = calculateSpace(pos, by))
        case p => p
      })
    } else {
      throw PlayerDoesntExistsException(name)
    }
  }

  /**
    * Here the next position is determined based on current position and the total dice roll. Because it's recursive
    * it can technically solve any combinations of rules. That means it could also end up in an infinite loop on a
    * badly designed board.
    *
    * Note that the recursive calls do some weird math by subtracting the "by" from the next position. This is done
    * so the "by" parameter can be passed on for use by the goose fields.
    *
    * @param from Starting position of the player.
    * @param by How many spaces the player will move.
    * @return The space the player ends up on.
    */
  private def calculateSpace(from: Int, by: Int): Int = {
    from + by match {
      case sum if sum > board.lastSpace =>
        // this could lead to an undefined state (negative position) if the field is smaller than the maximal dice roll
        calculateSpace(board.lastSpace -(sum - board.lastSpace) - by, by)
      case sum if board.gooseSpaces.contains(sum) =>
        calculateSpace(sum, by)
      case sum if board.bridges.get(sum).nonEmpty =>
        calculateSpace(board.bridges(sum) - by, by)
      case sum =>
        sum
    }
  }

  protected def nextState(players: Set[Player]): Game = {
    players.find(_.position == board.lastSpace)
      .map(p => FinishedGame(board, players, p))
      .getOrElse(RunningGame(board, players))
  }
}

case class RunningGame(board: Board, players: Set[Player]) extends Game
case class FinishedGame(board: Board, players: Set[Player], winner: Player) extends Game {
  override def nextState(players: Set[Player]): Game = this
}

object Game {
  def apply(players: Set[Player], board: Board): RunningGame = RunningGame(board, players)
  def apply(board: Board): RunningGame = RunningGame(board, Set.empty)
}

/**
  * A representation of a board the game can be played on.
  *
  * The [[Game]] assumes the board is "sane" and will not do any checks. This specifically means:
  *  - [[lastSpace]] must not be smaller than 12
  *  - bridges and goose spaces may not contain any infinite loops
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
