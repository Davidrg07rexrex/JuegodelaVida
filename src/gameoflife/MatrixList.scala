package gameoflife

class MatrixList[T](val rows: Int, val columns: Int, val data: List[T]):
  require(rows >= 0 && columns >= 0 && data.length == rows * columns,
    s"Una matriz $rows x $columns necesita ${rows * columns} datos (hay ${data.length})")

  private def index(p: Position): Int = p.row * columns + p.column
  
  private def positionOf(i: Int): Position = Position(i / columns, i % columns)

  def inside(p: Position): Boolean =
    p.row >= 0 && p.row < rows && p.column >= 0 && p.column < columns

  def get(p: Position): T =
    if !inside(p) then throw new IndexOutOfBoundsException("Fuera de índice")
    //Metodo de list
    else data(index(p))

  def update(p: Position, value: T): MatrixList[T] =
    if !inside(p) then throw new IndexOutOfBoundsException("Fuera de índice")
    //Metodo de list
    else new MatrixList(rows, columns, data.updated(index(p), value))

  def neighbors(p: Position): List[Position] =
    val offsets = List(-1, 0, 1)
    //usando flatMap y map
    offsets.flatMap(r => offsets.map(c => Position(p.row + r, p.column + c)))
      .filter(pos => pos != p && inside(pos)) //metodo filter

  def cells: List[(Position, T)] =
    //zip
    data.zipWithIndex.map((valor, i) => (positionOf(i), valor))

  def partition(f: T => Boolean): (List[Position], List[Position]) =
    //partition de list
    val (cumplen, noCumplen) = cells.partition((pos, valor) => f(valor))
    (cumplen.map(_._1), noCumplen.map(_._1))

  def filterNeighbors(p: Position, f: T => Boolean): List[Position] =
    //filter de list
    neighbors(p).filter(pos => f(get(pos)))

  
object MatrixList:
    def apply[T](rows: Int, columns: Int, data: List[T]): MatrixList[T] =
      new MatrixList(rows, columns, data)

    def apply[T](rows: Int, columns: Int, defaultValue: T): MatrixList[T] =
      new MatrixList(rows, columns, List.fill(rows * columns)(defaultValue)) //fill,inicializa con defaulValue

    def apply[T](rows: Int, columns: Int, defaultValue: T, marked: List[Position], markedValue: T): MatrixList[T] =
      new MatrixList(rows, columns,
        List.tabulate(rows * columns)(i =>    //tabulate,como fill pero con una funcion
          if marked.contains(Position(i / columns, i % columns)) then markedValue else defaultValue))