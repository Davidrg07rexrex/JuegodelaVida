package gameoflife
import scala.math.Ordering.Implicits.infixOrderingOps

class Game(val rows: Int, val columns: Int, val budget: Int, val maxGenerations: Int):

    def initialState: State =
        State(
            boards = Map(
                Player.Player1-> GameOfLifeMatrix.empty(rows, columns),
                Player.Player2-> GameOfLifeMatrix.empty(rows, columns),
            )
            remainingCells = Map(Player.Player1 -> budget,Player.Player2 -> budget)
            turn = Player.Player1
        )

    def hasFinished(state: State, player: Player): Boolean =
        state.remainingCells(player) == 0

    def placeCell(state: State, p: Position): Either[String, State] =
        val me = state.turn
        val board = state.boards(me)
        if hasFinished(state,me) then
            Left("Ya no te quedan células por colocar")
        else if !board.inside(p) then
            Left(s"La posición(${p.row},${p.column}) está fuera del tablero")
        else if board.get(p) == Alive then
            Left(s"La posición(${p.row},${p.column}) ya tiene una célula viva")
        else
            val rival = otherPlayer(me)
            val nextTurn = if hasFinished(state, rival) then me else rival
            Right(state.copy(
                boards = state.boards.update(me,board.update(p,Alive)),
                remainingCells = state.remainingCells.updated(me, state.remainingCells(me)-1),
                turn = nextTurn))
    def placementLoop(state: State, strategies: Map[Player, Strategy]): State = ??? //TODO
        @annotation.tailrec
        def aux(s:State, error: Option[String]): State =
            if hasFinished(s, Player.Player1) && hasFinished(s, Player.Player2) then s
            else if hasFinished(s, s.turn) then
                aux(s.copy(turn = otherPlayer(s.turn)), None)
            else
                val p = strategies(s.turn).choosePosition(s, s.turn, error)
                placeCells(s,p) match
                    case Right(next) => aux(next,None)
                    case Left(msg) => aux(s,Some(msg))

        aux(state, None)
    def simulate(board: GameOfLifeMatrix): (Int, Int) = ??? //TODO

    def determineWinner(resultPlayer1: (Int, Int), resultPlayer2: (Int, Int)): Option[Player] = ??? //TODO

    def play(strategies: Map[Player, Strategy]): GameResult = ??? //TODO
