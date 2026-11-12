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


//estrategia de la Ia
case class AIStrategy(chosenPattern:List[Position]) extends Strategy:
  override def choosePosition(state:State,player:Player,error:Option[String]):Position=
    val idx=chosenPattern.length-state.remainingCells(player)//saca el indice
    chosenPattern(idx)//devuelve posicion guardada

object AIStrategy:
  def apply(game:Game,attempts:Int):AIStrategy=
    val total=game.rows*game.columns
    val allPos=(0 until total).map(i=>Position(i/game.columns,i%game.columns)).toList//crea todas las posiciones

    @scala.annotation.tailrec
    def aux(restantes:Int,mejorPatron:List[Position],mejorNota:(Int,Int)):List[Position]=
      if(restantes<=0) mejorPatron//condicion parada
      else
        val patron=Random.shuffle(allPos).take(game.budget)//genera patron aleatorio
        val tablero=GameOfLifeMatrix.withAlive(game.rows,game.columns,patron)
        val nota=game.simulate(tablero)//evalua
        val esMejor=nota._1>mejorNota._1||(nota._1==mejorNota._1&&nota._2>mejorNota._2)//compara
        if(esMejor) aux(restantes-1,patron,nota)
        else aux(restantes-1,mejorPatron,mejorNota)

    val p0=Random.shuffle(allPos).take(game.budget)
    val t0=GameOfLifeMatrix.withAlive(game.rows,game.columns,p0)
    val n0=game.simulate(t0)
    AIStrategy(aux(attempts-1,p0,n0))
