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

    def simulate(board:GameOfLifeMatrix):(Int,Int)=
        @scala.annotation.tailrec
        def aux(b:GameOfLifeMatrix, gen:Int, history:Set[GameOfLifeMatrix]):(Int, Int)=
            if gen>=maxGenerations||history.contains(b) then
                (gen, b.aliveCount)
            else
                aux(b.nextGeneration,gen+1,history+b)

        aux(board,0,Set.empty)

    def determineWinner(resultPlayer1:(Int,Int),resultPlayer2:(Int,Int)):Option[Player]=
        val (g1, c1) = resultPlayer1
        val (g2, c2) = resultPlayer2
        if g1 > g2 then Some(Player.Player1)
        else if g2 > g1 then Some(Player.Player2)
        else if c1 > c2 then Some(Player.Player1)
        else if c2 > c1 then Some(Player.Player2)
        else None

    def play(strategies:Map[Player,Strategy]):GameResult=
        val endState=placementLoop(initialState,strategies)
        val r1=simulate(endState.boards(Player.Player1))
        val r2=simulate(endState.boards(Player.Player2))
        GameResult(endState,Map(Player.Player1 ->r1, Player.Player2 -> r2), determineWinner(r1, r2))
