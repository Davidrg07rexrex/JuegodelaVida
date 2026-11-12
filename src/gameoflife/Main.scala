package gameoflife

import scala.io.StdIn
import scala.util.Try

object Main extends App:
  val BoardSize      = 15
  val MaxGenerations = 1000

  @annotation.tailrec
  def readInt(prompt: String, min: Int, max: Int): Int =
    print(s"$prompt ($min-$max): ")
    StdIn.readLine().trim.toIntOption match
      case Some(n) if n >= min && n <= max => n
      case _ =>
        println("Valor no válido")
        readInt(prompt, min, max)
  
  //Pregunta al jugador si es IA o persona. True si es IA
  @annotation.tailrec
  def readHumanOrAI(prompt: String): Boolean = 
    print(s"$prompt (h=humano, i=IA): ")
    StdIn.readLine().trim.toLowerCase match 
      case "h" | "humano" => false
      case "i" | "ia" => true
      case _ => 
        println("Valor no válido")
        readHumanOrAI(prompt)
  
  //Pregunta si o no. Sí si devuelve true
  @annotation.tailrec
  def readYesNo(prompt: String): Boolean =
    print(s"$prompt (s/n): ")
    StdIn.readLine().trim.toLowerCase match
      case "s" | "si" | "sí" => true
      case "n" | "no" => false
      case _ =>
        println("Valor no válido")
        readYesNo(prompt)

  //Explica quien gana y porque
  def describeWinner(result: GameResult): String =
    val(gens1, alive1) = result.results(Player.Player1)
    result.winner match {
      case None =>
        s"Empate, los dos tableros se vuelven estables en la generación $gens1 con $alive1 células vivas."
      case Some(w) =>
        val (gensW, aliveW) = result.results(w)
        val (gensL, aliveL) = result.results(otherPlayer(w))
        val motivo =
          if gensW != gensL then 
            s"el tablero de $gensW tardo más generaciones en estabilizarse que el tablero de $gensL"
          else
            s"empataron en generaciones pero $gensW tenía más células que $gensL"
        s"Gana ${playerName(w)}: $motivo"  
    }

  println("EL JUEGO DE LA VIDA: la batalla de los patrones")

  val budget = readInt("Células por jugador", 1, BoardSize * BoardSize)
  val game = Game(BoardSize, BoardSize, budget, MaxGenerations)

  val strat1 = if readHumanOrAI("Jugador 1") then AIStrategy(game, 20) else ManualStrategy
  val strat2 = if readHumanOrAI("Jugador 2") then AIStrategy(game, 20) else ManualStrategy

  val strategies: Map[Player, Strategy] = Map(
    Player.Player1 -> strat1,
    Player.Player2 -> strat2
  )

  val result = game.play(strategies)

  println()
  println(result.finalState.boards(Player.Player1).show("Patrón inicial de Jugador 1"))
  println(result.finalState.boards(Player.Player2).show("Patrón inicial de Jugador 2"))
  Player.values.foreach: p =>
    val (gens, alive) = result.results(p)
    println(s"${playerName(p)}: $gens generaciones, $alive células vivas")
  println(describeWinner(result))

  if readYesNo("¿Quieres ver la simulación animada?") then
    Animation.animate(result)