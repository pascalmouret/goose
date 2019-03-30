package pascalmouret.goose

object Main extends App {
  private val moveRegex = "move ([^ ]+) ?([0-9]+)?".r
  private val addRegex = "add ([^ ]+)".r

  private val helpText =
    """Available commands:
      | - help
      |   Dispay this text.
      | - move <player> <spaces>?
      |   Move the player. Will either roll 2d6 or move given amount.
      | - add <player>
      |   Add a player with the given name.""".stripMargin

  println("Welcome to Goose. Type 'help' for... help.")
  waitForInput(
    Game(
      Board(
        lastSpace = 63,
        gooseSpaces = Set(5, 9, 14, 18, 23, 27),
        bridges = Map(6 -> 12)
      )
    )
  )

  private def waitForInput(game: RunningGame): Game = {
    print("> ")
    scala.io.StdIn.readLine() match {
      case "help" =>
        println(helpText)
        waitForInput(game)
      case moveRegex(player, null) =>
        processNewState(game.movePlayer(player))
      case moveRegex(player, by) =>
        processNewState(game.movePlayer(player, by.toInt))
      case addRegex(player) =>
        processNewState(game.addPlayer(player))
      case _ =>
        println("Unknown command. Try 'help'.")
        waitForInput(game)
    }
  }

  private def processNewState(game: Game): Game = {
    game.log.foreach(printEvent(_, game))
    game match {
      case g: RunningGame => waitForInput(g)
      case _ => sys.exit(0)
    }
  }

  private def printEvent(event: Event, game: Game): Unit = {
    println(
      event match {
        case PlayerLandedOn(player, space) =>
          s"$player ended up on $space."
        case PlayerLandedOnGoose(player, space) =>
          s"$player landed on the goose field $space. Moving again."
        case PlayerTraversedBridge(player, from, to) =>
          s"$player traversed bridge from $from to $to."
        case PlayerBounced(player) =>
          s"$player bounced back from the end of the board."
        case PlayerWon(player) =>
          s"$player has won the game."
        case PlayerAdded(player) =>
          s"$player was added to the game. Current players: ${game.players.map(_.name).mkString(", ")}."
        case PlayerRolledDice(player, total) =>
          s"$player rolled $total."
      }
    )
  }
}
