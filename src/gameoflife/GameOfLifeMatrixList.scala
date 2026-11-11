package gameoflife

case class GameOfLifeMatrixList(override val rows: Int, override val columns: Int, override val data: List[Cell])
  extends MatrixList[Cell](rows, columns, data):

    override def update(p: Position, value: Cell): GameOfLifeMatrixList =
        val m=super.update(p,value)
        //crea nuevo GameOfLifeMatrixList con la data de super.update
        GameOfLifeMatrixList(rows, columns, m.data)

    def aliveCells: List[Position] =
        //partition de MatrixList
        val (vivas, _) = partition(_ == Alive)
        vivas

    def aliveCount: Int =
        //length de list
        aliveCells.length

    def countAliveNeighbors(p: Position): Int =
        //filterNeighbors de MatrixList
        filterNeighbors(p, _ == Alive).length

    def nextGeneration: GameOfLifeMatrixList =
        //map de list sobre cells para aplicar reglas B3/S23
        val newData = cells.map: (pos, cell) =>
            val vivas = countAliveNeighbors(pos)
            cell match
                case Alive => if vivas == 2 || vivas == 3 then Alive else Dead
                case Dead  => if vivas == 3 then Alive else Dead
        GameOfLifeMatrixList(rows, columns, newData)

    def toMatrix: GameOfLifeMatrix = GameOfLifeMatrix(rows, columns, data)
    override def toString: String = toMatrix.show()

object GameOfLifeMatrixList:
    def empty(rows: Int, columns: Int): GameOfLifeMatrixList =
        //apply de MatrixList con fill por debajo
        val m = MatrixList[Cell](rows, columns, Dead)
        GameOfLifeMatrixList(rows, columns, m.data)

    def withAlive(rows: Int, columns: Int, alive: List[Position]): GameOfLifeMatrixList =
        //apply de MatrixList con tabulate y marked por debajo
        val m = MatrixList[Cell](rows, columns, Dead, alive, Alive)
        GameOfLifeMatrixList(rows, columns, m.data)