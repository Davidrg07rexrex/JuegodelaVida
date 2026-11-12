package gameoflife

import scala.io.StdIn
import scala.util.{Random, Try}

trait Strategy:
  def choosePosition(state: State, player: Player, error: Option[String]): Position

case object ManualStrategy extends Strategy:
  def choosePosition(state: State, player: Player, error: Option[String]): Position =
    //se imprime por pantalla el error si existe
    error match
      case Some(msg) => println(s"Error$msg")
      case None => ()

    val title = s"${playerName(player)} · te quedan ${state.remainingCells(player)} células"
    println(state.boards(player).show(title))
    readPosition()

  //pide "fila columna" y se repite hasta que el formato sea correcto
  @annotation.tailrec
  private def readPosition(): Position =
    print("Posición (fila columna): ")
    val line = StdIn.readLine()
    if line == null then
      println("Entrada cerrada. Fin de la partida.")
      sys.exit(0)
    else
      parsePosition(line) match
        case Some(p) => p
        case None =>
          println("Formato no válido: escribe dos números enteros, por ejemplo: 2 1")
          readPosition()

  private def parsePosition(line: String): Option[Position] =
    line.trim.split("\\s+") match
      case Array(r, c) =>
        r.toIntOption match
          case None => None
          case Some(row) =>
            c.toIntOption match
              case None => None
              case Some(col) => Some(Position(row,col))


      case _ => None