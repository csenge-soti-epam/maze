import FieldType.FieldType

import scala.annotation.tailrec
import RandomVectorHelper._
import Test.{colSize, maze, rowSize}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

case class Maze(fieldMatrix: Array[Array[FieldType]]) {

  var possiblePaths: Array[(Int, Int)] = Array.empty

  def draw(currentMatrix: Array[Array[FieldType]]): Unit = {
    currentMatrix.foreach(row => println(row.map(FieldType.getPrintable).mkString("")))
  }

  def calculateShortestPath(): Future[Unit] = {
    calculatePathFromCell((0, 1), (14, 13), Vector.empty, fieldMatrix).map(_.foreach(println))
  }

  //@tailrec
  final def calculatePathFromCell(startParams: (Int, Int), aimParams: (Int, Int), oldParams: Vector[(Int, Int)], currentMatrix: Array[Array[FieldType]]): Future[Vector[(Int, Int)]] = {
    if (startParams == aimParams) {
      currentMatrix(startParams._1)(startParams._2) = FieldType.Road
      println("------------------------------" + startParams, aimParams, oldParams)
      possiblePaths :+ oldParams
      Future.successful(Vector.empty)
    } else {
      val nextParams: Seq[((Int, Int), Vector[(Int, Int)])] = visitCellAndGetNextPathParams(startParams, oldParams, currentMatrix)
      println("+++++++++++++++++++++++++++++" + nextParams.map(_._1))
      val tmp = currentMatrix.clone()
      val futures: Seq[Future[Vector[(Int, Int)]]] = nextParams.map(np =>
        calculatePathFromCell(np._1, aimParams, np._2, tmp))

      val fSerialized = {
        var fAccum: Future[Vector[(Int, Int)]] = Future(scala.collection.immutable.Vector.empty)
        for(item <- futures) {
          fAccum = fAccum.flatMap[Vector[(Int, Int)]] ( _ => {
            item
          } )
        }
        fAccum
      }

      fSerialized


      //Future.sequence(nextParams.map(np =>
        //calculatePathFromCell(np._1, aimParams, np._2, currentMatrix.clone()))).map(_.flatten.toVector)
    }
  }

  private def visitCellAndGetNextPathParams(currentCellParams: (Int, Int), oldParams: Vector[(Int, Int)], currentMatrix: Array[Array[FieldType]]): Vector[((Int, Int), Vector[(Int, Int)])] = {
    currentMatrix(currentCellParams._1)(currentCellParams._2) = FieldType.Road
    getNextRoadCells(currentCellParams, currentMatrix) match {
      case head +: tail => (head, oldParams :+ currentCellParams) +: tail.map((_, oldParams :+ currentCellParams))
      case IndexedSeq() => {
        oldParams.lastOption.map(lastCellParams => {
          currentMatrix(currentCellParams._1)(currentCellParams._2) = FieldType.RoadVisited
          (lastCellParams, oldParams.take(oldParams.size - 1))
        }).toVector
      }
    }
  }


  private def getNextRoadCells(params: (Int, Int), currentMatrix: Array[Array[FieldType]]): Vector[(Int, Int)] = {
    (for {
      a_ <- Vector(params._1, params._1 + 1, params._1 - 1).filter(x => x <= currentMatrix.length && x >= 0)
      b_ <- Vector(params._2, params._2 + 1, params._2 - 1).filter(x => x <= currentMatrix.indices.length && x >= 0)}
      yield (a_, b_))
      .filter(xy => (xy == params) || !(xy._1 != params._1 && xy._2 != params._2))
      .filter(xy => ((currentMatrix(xy._1)(xy._2) != FieldType.Wall) && (currentMatrix(xy._1)(xy._2) != FieldType.Road) && (currentMatrix(xy._1)(xy._2) != FieldType.RoadVisited)))
  }
}

object Maze {

  def apply(rowSize: Int, colSize: Int): Maze = {
    val matrix = getBaseMatrix(rowSize, colSize)
    calculateMazeFromCell(matrix, (1, 1), Vector.empty)

    val rowNr = matrix.getRandomIndex().get
    val rowNr2 = matrix.getRandomIndex().get
    matrix(rowNr)(matrix(rowNr).getRandomIndexWithFilter(_ == FieldType.Wall).get) = FieldType.WallBroke
    matrix(rowNr2)(matrix(rowNr2).getRandomIndexWithFilter(_ == FieldType.Wall).get) = FieldType.WallBroke
    Maze(matrix)
  }

  private def getBaseMatrix(rowSize: Int, colSize: Int): Array[Array[FieldType]] = {
    val matrix: Array[Array[FieldType]] = Array.ofDim[FieldType](rowSize, colSize)
    for {
      i <- matrix.indices
      j <- 0 until colSize
    } yield {
      if ((i == 0 && j == 1) || (i == rowSize - 1 && j == colSize - 2)) {
        matrix(i)(j) = FieldType.Cell
      } else matrix(i)(j) = getBaseFieldType(i, j)
    }
    matrix
  }

  @tailrec
  private def calculateMazeFromCell(matrix: Array[Array[FieldType]], params: (Int, Int), oldParams: Vector[(Int, Int)]): Option[(Int, Int)] = {
    val nextParams = visitCellAndGetNextMazeParams(matrix, params, oldParams)
    if (nextParams.isEmpty) {
      None
    } else {
      calculateMazeFromCell(matrix, nextParams.get._1, nextParams.get._2)
    }
  }

  private def visitCellAndGetNextMazeParams(matrix: Array[Array[FieldType]], currentCellParams: (Int, Int), oldParams: Vector[(Int, Int)]): Option[((Int, Int), Vector[(Int, Int)])] = {
    matrix(currentCellParams._1)(currentCellParams._2) = FieldType.CellVisited
    getNextCell(matrix, currentCellParams)
      .map((nextCellParams: (Int, Int)) => {
        matrix((nextCellParams._1 + currentCellParams._1) / 2)((nextCellParams._2 + currentCellParams._2) / 2) = FieldType.WallBroke
        (nextCellParams, oldParams :+ currentCellParams)
      }).orElse(
      oldParams.lastOption.map(lasCellParams => {
        (lasCellParams, oldParams.take(oldParams.size - 1))
      })
    )
  }

  private def getNextCell(matrix: Array[Array[FieldType]], params: (Int, Int)): Option[(Int, Int)] = {
    (for {
      a_ <- Vector(params._1, params._1 + 2, params._1 - 2).filter(x => x < matrix.length && x >= 0)
      b_ <- Vector(params._2, params._2 + 2, params._2 - 2).filter(x => x < matrix.indices.length && x >= 0)}
      yield (a_, b_))
      .filter(xy => (xy == params) || !(xy._1 != params._1 && xy._2 != params._2))
      .filter(xy => ((matrix(xy._1)(xy._2) != FieldType.CellVisited)))
      .getRandom()
  }

  private def getBaseFieldType(rowNr: Int, colNr: Int) = {
    (rowNr % 2, colNr % 2) match {
      case (0, _) => FieldType.Wall
      case (1, 0) => FieldType.Wall
      case (1, 1) => FieldType.Cell
      case _ => FieldType.Cell
    }
  }
}
