package pascalmouret.goose

import org.specs2.mutable.Specification

class GameSpec extends Specification {
  "Game.addPlayer" should {
    "add as many player as wanted" in {
      Game(Board(10, Set.empty, Map.empty))
        .addPlayer("Gander")
        .addPlayer("Gabble")
        .addPlayer("Goose")
        .players mustEqual Set(Player("Gander"), Player("Gabble"), Player("Goose"))
    }

    "detect duplicate players" in {
      Game(Board(10, Set.empty, Map.empty))
        .addPlayer("Gander")
        .addPlayer("Gander") must throwA[PlayerExistsException]
    }
  }

  "Game.movePlayer" should {
    "check if a player exists" in {
      Game(Board(10, Set.empty, Map.empty))
        .movePlayer("Goose", 3) must throwA[PlayerDoesntExistsException]
    }

    "move a player" in {
      val game = Game(Set(Player("Gabble"), Player("Gander")), Board(10, Set.empty, Map.empty))
        .movePlayer("Gabble", 2)
        .movePlayer("Gander", 2)
        .movePlayer("Gabble", 3)

      game.playerPosition("Gabble") mustEqual 5
      game.playerPosition("Gander") mustEqual 2
    }

    "bounce a player back if the last space is missed" in {
      Game(Set(Player("Gabble")), Board(10, Set.empty, Map.empty))
        .movePlayer("Gabble", 12)
        .playerPosition("Gabble") mustEqual 8
    }

    "move same distance again when player lands on goose field" in {
      Game(Set(Player("Gabble")), Board(10, Set(2, 4, 8), Map.empty))
        .movePlayer("Gabble", 2)
        .playerPosition("Gabble") mustEqual 6
    }

    "traverse bridges" in {
      Game(Set(Player("Gabble")), Board(10, Set(8), Map(1 -> 4, 4 -> 3, 3 -> 8)))
        .movePlayer("Gabble", 1)
        .playerPosition("Gabble") mustEqual 9
    }
  }
}
