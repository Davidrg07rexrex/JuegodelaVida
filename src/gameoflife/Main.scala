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

  def readHumanOrAI(prompt: String): Boolean = ??? //TODO

  @annotation.tailrec
  def readYesNo(prompt: String): Boolean =
    print(s"$prompt (s/n): ")
    StdIn.readLine().trim.toLowerCase match
      case "s" | "si" | "sí" => true
      case "n" | "no" => false
      case _ =>
        println("Valor no válido")
        readYesNo(prompt)

  def describeWinner(result: GameResult): String = ??? //TODO
  println("EL JUEGO DE LA VIDA: la batalla de los patrones")
  ??? //TODO pedir el numero de celulas a poner, elegir las estrategias, jugar, animar y mostrar el resultado
