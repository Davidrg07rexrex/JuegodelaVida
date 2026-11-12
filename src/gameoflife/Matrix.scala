package gameoflife

class Matrix[T](val rows: Int, val columns: Int, val data: List[T]):
  require(rows >= 0 && columns >= 0 && data.length == rows * columns,
    s"Una matriz $rows x $columns necesita ${rows * columns} datos (hay ${data.length})")

  private def index(p: Position): Int = p.row * columns + p.column //calcula indice lineal

  private def positionOf(i: Int): Position = Position(i / columns, i % columns) //convierte indice a posicion

  def inside(p: Position): Boolean =
    p.row>=0&&p.row<rows&&p.column>=0&&p.column<columns //comprueba si esta dentro

  def get(p: Position): T =
    if (!inside(p)) then throw new IndexOutOfBoundsException("Fuera de indice")
    else
      @scala.annotation.tailrec
      def getRec(listaRestante: List[T], PasosRestantes: Int): T = listaRestante match
        case Nil =>
          throw new IndexOutOfBoundsException("Indice fuera de rango.")
        case head :: tail => if (PasosRestantes == 0) head  else getRec(tail, PasosRestantes-1) //recursiva para buscar valor

      getRec(data, index(p))

  def update(p: Position, value: T): Matrix[T] =
    if (!inside(p)) then throw new IndexOutOfBoundsException("Fuera de indice")
    else
      @scala.annotation.tailrec
      def updateRec(listaRestante:List[T],PasosRestantes:Int,acc:List[T]):List[T]= listaRestante match
        case Nil=> acc.reverse
        case head::tail=>
          if (PasosRestantes==0) then acc.reverse++(value::tail) //reemplaza valor
          else updateRec(tail,PasosRestantes-1, head::acc)
      new Matrix(rows,columns,updateRec(data,index(p),Nil))

  def neighbors(p: Position): List[Position] =
    val posibles=List(-1,0,1)
    @scala.annotation.tailrec
    def aux(row:List[Int],column:List[Int],acc:List[Position]):List[Position]= (row, column) match
      case (Nil, _) => acc.reverse
      case (_ :: tail, Nil) => aux(tail, posibles, acc)
      case (r1 :: _, c1 :: cTail) =>
        val neighborPos = Position(p.row + r1, p.column + c1)
        val isValid = (r1 != 0 || c1 != 0) && inside(neighborPos)
        val nextAcc = if isValid then neighborPos :: acc else acc
        aux(row, cTail, nextAcc) //calcula vecinos

    aux(posibles, posibles, Nil)

  def partition(f: T => Boolean): (List[Position], List[Position]) =
    @scala.annotation.tailrec
    def aux(listaRestante: List[T], index: Int, cumplen: List[Position], noCumplen: List[Position]): (List[Position], List[Position]) =
      listaRestante match
        case Nil =>
          (cumplen.reverse, noCumplen.reverse)
        case head :: tail =>
          val pos = positionOf(index)
          if f(head) then aux(tail, index + 1, pos :: cumplen, noCumplen) //separa segun condicion
          else aux(tail, index + 1, cumplen, pos :: noCumplen)

    aux(data, 0, Nil, Nil)


  def filterNeighbors(p: Position, f: T => Boolean): List[Position] =
    @scala.annotation.tailrec
    def aux(vecinosRestantes: List[Position], acc: List[Position]): List[Position] =
      vecinosRestantes match
        case Nil => acc.reverse
        case pos :: tail =>
          if f(get(pos)) then aux(tail, pos :: acc) //filtra vecinos
          else aux(tail, acc)

    aux(neighbors(p), Nil)


object Matrix:

  def apply[T](rows: Int, columns: Int, data: List[T]): Matrix[T] =
    new Matrix(rows, columns, data)

  def apply[T](rows: Int, columns: Int, defaultValue: T): Matrix[T] =
    @scala.annotation.tailrec
    def aux(pasosRestantes: Int, acc: List[T]): List[T] =
      if pasosRestantes == 0 then acc
      else aux(pasosRestantes - 1, defaultValue :: acc) //rellena por defecto

    new Matrix(rows, columns, aux(rows * columns, Nil))

  def apply[T](rows: Int, columns: Int, defaultValue: T, marked: List[Position], markedValue: T): Matrix[T] =
    val total = rows * columns

    @scala.annotation.tailrec
    def aux(index: Int, acc: List[T]): List[T] =
      if index < 0 then acc
      else
        val pos = Position(index / columns, index % columns)
        val valor = if marked.contains(pos) then markedValue else defaultValue
        aux(index - 1, valor :: acc) //construye con marcados

    new Matrix(rows, columns, aux(total - 1, Nil))