package year2021.day6

object RecursiveSolution {
  def calculateGenerationSize(initialGen: Seq[Int], daysToSimulate: Int): Long = {
    initialGen
      .map(specie => calculateSuccessorSize(specie, daysToSimulate))
      .sum
  }

  private def calculateSuccessorSize(daysUntilSpawn: Int, daysToSimulate: Int): Long = {
    if (daysToSimulate <= 0) {
      1
    } else if (daysUntilSpawn <= 0) {
      calculateSuccessorSize(6, daysToSimulate - 1) + calculateSuccessorSize(8, daysToSimulate - 1)
    } else {
      calculateSuccessorSize(daysUntilSpawn - 1, daysToSimulate - 1)
    }
  }
}
