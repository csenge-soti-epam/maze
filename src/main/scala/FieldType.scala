object FieldType extends Enumeration {
  type FieldType = Value
  val Wall, Cell, CellVisited, RoadVisited, WallBroke, Road = Value

  def getPrintable(field: FieldType) = {
    field match {
      case FieldType.Wall => Console.WHITE_B + "   " + Console.RESET
      case FieldType.Cell | FieldType.CellVisited | FieldType.RoadVisited | FieldType.WallBroke => Console.RESET + "   "
      case FieldType.Road => " * "
    }
  }
}
