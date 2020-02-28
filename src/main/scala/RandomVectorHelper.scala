import scala.util.Random

object RandomVectorHelper {

  implicit class RandomVector[A](val self: Vector[A]) extends AnyVal {
    def getRandom(): Option[A] = if (self.size == 0) None else Some(self(Random.nextInt(self.size)))

    def getRandom(filterFn: A => Boolean): Option[A] = if (self.size == 0) None else Some(self.filter(filterFn)(Random.nextInt(self.size)))
  }

  implicit class RandomArray[A](val self: Array[A]) extends AnyVal {
    def getRandom(): Option[A] = if (self.size == 0) None else Some(self(Random.between(0, self.size)))

    def getRandomIndex(): Option[Int] = if (self.size == 0) None else Some(Random.between(1, self.size - 2))


    def getRandomIndexWithFilter(filterFn: A => Boolean): Option[Int] = {
      val filtered = self.zipWithIndex.filter(tuple => filterFn(tuple._1)).map(_._2)
      if (filtered.size == 0) None else Some(filtered(Random.between(1, filtered.size - 1)))
    }

  }

}
