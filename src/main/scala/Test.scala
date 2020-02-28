import FieldType.FieldType
import scala.annotation.tailrec
import scala.util.Random
import RandomVectorHelper._

object Test extends App {

  private val rowSize: Int = 15
  private val colSize: Int = 15
  private val maze = Maze(rowSize, colSize)
  val startTime = System.currentTimeMillis()
  maze.calculatePathFromCell((0, 1), (rowSize - 1, colSize - 2), Vector.empty, maze.fieldMatrix)

  println("Elapsed time:" + (System.currentTimeMillis() - startTime))


}

